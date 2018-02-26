(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
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

    let to_bin_v v = match v with
          0 -> false
          | _ -> true

    let to_int_v v = match v with
          true -> 1
        | false -> 0

    let binop operation v1 v2 = match operation with
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
        |  _ -> failwith (Printf.sprintf "%s not implemented yet" operation)
    

    let rec eval state expr = 
    match expr with
        | Const i -> i
        | Var x -> state x
        | Binop (operation, e1, e2) ->
            let v1 = eval state e1 in
            let v2 = eval state e2 in
            binop operation v1 v2
    
end
                    
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

       Takes a configuration and a statement, and returns another configuration *)
                                                         
  (* end *)

(* The top-level definitions *)
(* The top-level syntax category is statement *)
(* type t = Stmt.t     *)

(* Top-level evaluator
     eval : int list -> t -> int list
   Takes a program and its input stream, and returns the output stream
*)
let rec eval c pr = match c, pr with
    | (s, z::i, o), Read v -> (Expr.update v z s,i,o)
    | (s,i,o), Write e -> (s,i,o @ [Expr.eval s e])
    | (s,i,o), Assign (v, e) -> (Expr.update v (Expr.eval s e) s,i,o)
    | c, Seq (e1, e2) -> eval (eval c e1) e2


end
(* let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o *)
