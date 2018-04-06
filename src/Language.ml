(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
open List
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let femp x = failwith (Printf.sprintf "Undefined variable %s" x)
    let empty = {g = femp; l = femp; scope = []} 
    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v ({g; l; scope} as s) = if List.mem x scope then
    {g; l = (fun y -> if x = y then v else l y); scope}
    else {g = (fun y -> if x = y then v else g y); l; scope}
                                
    (* Evals a variable in a state w.r.t. a scope *)
    let eval ({g; l; scope} as s) x = if List.mem x scope then l x else g x

    (* Creates a new scope, based on a given state *)
    let enter ({g; _; _} as st) xs = {g; l = femp; scope = xs}

    (* Drops a scope *)
    let leave ({g; _; _} as st) ({_; l; scope} as st') = {g; l; scope}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)      

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
  ostap (                                      
      parse:
    !(Ostap.Util.expr 
             (fun x -> x)
       (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
    `Lefta, ["!!"];
    `Lefta, ["&&"];
    `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
    `Lefta, ["+" ; "-"];
    `Lefta, ["*" ; "/"; "%"];
              |] 
       )
       primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"

    )
        
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters, local variables, and a body for given definition
    *)
    let rec eval env ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x       -> (match i with z::i' -> (State.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e       -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)   -> (State.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2) -> eval env (eval env conf s1) s2
      | Skip ->  conf
      | If (e, s1, s2) -> (match Expr.eval st e with
        | 0 -> eval env conf s2
        | _ -> eval env conf s1)
      | While (e, s) -> (match Expr.eval st e with
        | 0 -> conf
        | _ -> eval env (eval env conf s) stmt)
      | Repeat (s, e) as repUn -> (
                let cs = eval env conf s in
                let (est, ixs, oxs) = cs in
                    match (Expr.eval est e) with
                    | 0 -> eval env cs repUn
                    | _ -> cs
                  )
      | Call (f, args) -> let (params, locals, body) = env#definition f in
        let evaled_args = List.map (Expr.eval st) args in
        let entered_st = State.enter st (params @ locals) in
        let prepared_state = List.fold_left2 (fun statement args' values -> State.update args' values statement) entered_st params evaled_args in
        let (st', i', o') = eval env (prepared_state, i, o) body in
        (State.leave st' st, i', o')        
      
let elif_branch elif els =
      let last_action = match els with
        | None -> Skip
        | Some act -> act
      in fold_right (fun (cond, action) branch -> If (cond, action, branch)) elif last_action
                                
    (* Statement parser *)
    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;
      stmt:
        %"read" "(" x:IDENT ")"          {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | x:IDENT ":=" e:!(Expr.parse)    {Assign (x, e)}
      | %"skip" {Skip}
      | %"if" e:!(Expr.parse) %"then" s1:parse 
        elifs:(%"elif" !(Expr.parse) %"then" parse)* 
        els:(%"else" parse)? %"fi"
        {If (e, s1, elif_branch elifs els)}
      | %"while" e:!(Expr.parse) %"do" s1:parse %"od" {While (e, s1)} 
      | %"repeat" s1:parse %"until" e:!(Expr.parse) {Repeat(s1,e)}
      | %"for" e1:parse "," e:!(Expr.parse) "," s1:parse %"do" s2:parse %"od" {Seq(e1, While(e, Seq(s2,s1)))}
      (* | name:IDENT "(" args:(!(Util.list0)[Expr.parse])? ")" {Call (name, args)}  *)
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      parse: %"fun" name:IDENT "(" args:!(Util.list)[ostap (IDENT)]? ")"
        locals: (%"local" !(Util.list)[ostap (IDENT)])? "{"
        body: !(Stmt.parse) "}" {(name, (args, locals, body))}
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i = failwith "Not implemented"
                                   
(* Top-level parser *)
let parse = failwith "Not implemented"
