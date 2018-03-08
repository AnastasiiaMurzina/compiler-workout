(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
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
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
<<<<<<< HEAD
    *)
    let to_bin_v v = match v with
      0 -> false
    | _ -> true

    let to_int_v v = match v with
        true -> 1
    | _ -> 0

    let binop operation v1 v2 = 
    match operation with
    | "+" -> v1 + v2
    | "-" -> v1 - v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2
    | "%" -> v1 mod v2
    | "<" -> to_int_v (v1 < v2)
    | "<=" -> to_int_v (v1 <= v2)
    | ">" -> to_int_v (v1 > v2)
    | ">=" -> to_int_v (v1 >= v2)
    | "==" -> to_int_v (v1 = v2)
    | "!=" -> to_int_v (v1 <> v2)
    | "&&" -> to_int_v (to_bin_v v1 && to_bin_v v2)
    | "!!" -> to_int_v (to_bin_v v1 || to_bin_v v2)
    | _ -> failwith (Printf.sprintf "%s not implemented yet" operation)  
    
    let rec eval state expr = 
    match expr with
    | Const i -> i
    | Var x -> state x
    | Binop (operation, e1, e2) ->
        let v1 = eval state e1 in
        let v2 = eval state e2 in
        binop operation v1 v2
=======
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
      | Var   x -> st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)
>>>>>>> a02d2f10f0fb937b9c20ce1bc554e66439d1508e

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
<<<<<<< HEAD
 let conv_op s = fun x y -> Binop(s, x, y)
 let ostap_bin ops = List.map (fun s -> (ostap ($(s)), conv_op s)) ops

  ostap (
    primary: x:IDENT {Var x} | x:DECIMAL {Const x} | -"(" parse -")";
    parse: !(Ostap.Util.expr
               (fun x -> x)
               [|
                `Lefta, ostap_bin ["!!"];
                 `Lefta, ostap_bin ["&&"];
                 `Nona,  ostap_bin [">="; ">"; "<="; "<"; "=="; "!="];
                 `Lefta, ostap_bin ["+"; "-"];
                 `Lefta, ostap_bin ["*"; "/"; "%"]
               |]
              primary
            )
  )
end
=======
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
>>>>>>> a02d2f10f0fb937b9c20ce1bc554e66439d1508e
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
<<<<<<< HEAD
    let rec eval conf st = match conf, st with
    | (state, instream, outstream), (Assign (x, expr)) -> (Expr.update x (Expr.eval state expr) state, instream, outstream)
    | (state, z::instream, outstream), (Read x) -> (Expr.update x z state, instream, outstream)
    | (state, instream, outstream), (Write e) -> (state, instream, outstream @ [Expr.eval state e])
    | config, (Seq (s1, s2)) -> eval (eval config s1) s2

    (* Statement parser *)
    ostap (
      parse: seq | stmt;
      stmt: assign | read | write;
      assign: variable:IDENT -":=" expr:!(Expr.parse) {Assign (variable, expr)};
      read: "read" -"(" variable:IDENT -")" {Read variable};
      write: "write" -"(" expr:!(Expr.parse) -")" {Write expr};
      seq: left_stmt:stmt -";" right_stmt:parse {Seq (left_stmt, right_stmt)}
=======
    let rec eval ((st, i, o) as conf) stmt =
      match stmt with
      | Read    x       -> (match i with z::i' -> (Expr.update x z st, i', o) | _ -> failwith "Unexpected end of input")
      | Write   e       -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e)   -> (Expr.update x (Expr.eval st e) st, i, o)
      | Seq    (s1, s2) -> eval (eval conf s1) s2
                                
    (* Statement parser *)
    ostap (
      parse:
        s:stmt ";" ss:parse {Seq (s, ss)}
      | stmt;
      stmt:
        "read" "(" x:IDENT ")"          {Read x}
      | "write" "(" e:!(Expr.parse) ")" {Write e}
      | x:IDENT ":=" e:!(Expr.parse)    {Assign (x, e)}            
>>>>>>> a02d2f10f0fb937b9c20ce1bc554e66439d1508e
    )

      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     