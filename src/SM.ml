open GT       
open Syntax
open List
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *) 

let extra_eval conf pr = match conf, pr with
| (y::x::st, c), BINOP operation -> ((Syntax.Expr.binop operation x y)::st, c)
| (st, c), CONST z -> (z::st, c)
| (st, (s, z::i, o)), READ -> (z::st,(s,i,o))
| (z::st, (s,i,o)), WRITE -> (st,(s,i,o @ [z]))
| (st, (s,i,o)), LD x -> ((s x):: st, (s,i,o))
| (z::st, (s,i,o)), ST x -> (st, (Syntax.Expr.update x z s,i,o))
| _, _ -> failwith "Unexpected instructions" 

let rec eval config prog = match config, prog with
   | c, [] -> c 
   | c, i::p -> eval (extra_eval c i) p 
(* Top-level evaluation
     val run : int list -> prg -> int list
   Takes an input stream, a program, and returns an output stream this program calculates
*)
(* let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o *)

let rec ex_comp = function
    | Syntax.Expr.Const z -> [CONST z]
    | Syntax.Expr.Var v -> [LD v]
    | Syntax.Expr.Binop (operation, e1, e2) -> ex_comp e1 @ ex_comp e2 @ [BINOP operation]

(* Stack machine compiler
     val compile : Syntax.Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile (tpst: Syntax.Stmt.t) : prg = match tpst with
   | Stmt.Assign (z, e) -> (ex_comp e) @ [ST z]
   | Stmt.Read z -> [READ ; ST z]
   | Stmt.Write e -> (ex_comp e) @ [WRITE]
   | Stmt.Seq (e1, e2) -> compile e1 @ compile e2
