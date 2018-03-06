open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let step_eval config prg = match config, prg with
 | (y::x::st, conf), BINOP p -> ((Language.Expr.binop p x y)::st, conf)
 | (st, conf), CONST z -> (z::st, conf)
 | (st, (state, z::instream, outstream)), READ -> (z::st, (state, instream, outstream))
 | (z::st, (state, instream,outstream)), WRITE -> (st, (state, instream, outstream @ [z]))
 | (st, (state, instream,outstream)), LD x -> ((state x)::st, (state, instream,outstream))
 | (z::st, (state, instream, outstream)), ST x -> (st, (Language.Expr.update x z state, instream, outstream))
 | _ , _ -> failwith "Unexpected"


 let rec eval = List.fold_left step_eval
(* let rec eval config = function
 | [] -> config
| i::prg -> eval (step_eval config i) prg *)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec step_compile = function
 | Expr.Var x -> [LD x]
 | Expr.Const n -> [CONST n]
 | Expr.Binop (op, a, b) -> step_compile a @ step_compile b @ [BINOP op] 

let rec compile  = function
 | Stmt.Assign (x, e) -> step_compile e @ [ST x] 
 | Stmt.Read x -> [READ; ST x]
 | Stmt.Write e -> step_compile e @ [WRITE]
 | Stmt.Seq (s1, s2) -> compile s1 @ compile s2

                         
