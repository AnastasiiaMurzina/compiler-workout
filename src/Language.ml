(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
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

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)

    ostap (
      parse: empty {failwith "Not implemented yet"}
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
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval conf st = match conf, st with
    | (state, instream, outstream), Assign (x, expr) -> (Expr.update x (Expr.eval state expr) state, instream, outstream)
    | (state, z::instream, outstream), Read x -> (Expr.update x z state, instream, outstream)
    | (state, instream, outstream), Write e -> (state, instream, outstream @ [Expr.eval state e])
    | (state, instream, outstream), Seq (s1, s2) -> eval (eval (state, instream, outstream) s1) s2

    (* Statement parser *)
    ostap (
      parse: empty {failwith "Not implemented yet"}
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
