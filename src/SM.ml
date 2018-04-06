open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let checkCJump cond value = match cond with
| "nz" -> value <> 0
| "z" -> value = 0 

let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
| [] -> conf
| JMP l :: _ -> eval env conf (env#labeled l)
| CJMP (znz, l) :: prg' -> let x::stack' = stack in let z = checkCJump znz in
        if z x then eval env conf (env#labeled l) else eval env conf prg'
| insn :: prg' ->  (* let c' = *)
   (match insn with
      | BINOP op -> let y::x::stack' = stack in eval env (cstack, Expr.to_func op x y :: stack', c) prg'
      | READ     -> let z::i'        = i     in eval env (cstack, z::stack, (st, i', o)) prg'
      | WRITE    -> let z::stack'    = stack in eval env (cstack, stack', (st, i, o @ [z])) prg'
      | CONST i  -> eval env (cstack, i::stack, c) prg'
      | LD x     -> eval env (cstack, (st x) :: stack, c) prg'
      | ST x     -> let z::stack' = stack in eval env (cstack, stack', (State.update x z st, i, o)) prg' (*fix update*)
      | LABEL _ -> eval env conf prg') 
       (* in eval env c' prg'   *)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let compile (defs, p) = failwith "Not implemented"
(* let rec compile stmt = *)
let labels = object
val num = 0
method incr = "label_" ^ string_of_int num, {< num = num + 1 >}
end in
let rec expr = function
  | Expr.Var x -> [LD x]
  | Expr.Const n -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
    in let rec l_compile p = function
  | Stmt.Seq (s1, s2) -> let p, first = l_compile p s1 in
    let p, second = l_compile p s2 in
      p, first @ second
  | Stmt.Read x -> p, [READ; ST x]
  | Stmt.Write e -> p, expr e @ [WRITE]
  | Stmt.Assign (x, e) -> p, expr e @ [ST x]
  | Stmt.Skip -> p, []
  | Stmt.If (e, s1, s2) -> let flbl, p = p#incr in
      let endlbl, p = p#incr in
      let p, if1 = l_compile p s1 in
      let p, if2 = l_compile p s2 in
      let rec instr = match s2 with
        | Skip -> [LABEL flbl]
        | _ -> [JMP endlbl; LABEL flbl] @ if2 @ [LABEL endlbl] in
          p, (expr e) @ [CJMP ("z", flbl)] @ if1 @ instr
        | Stmt.While (e,s) -> let initlbl, p = p#incr in
          let endlbl, p = p#incr in
          let p, body = l_compile p s in
          p, [LABEL initlbl] @ (expr e) @ [CJMP ("z", endlbl)] @ body @ [JMP initlbl; LABEL endlbl]
        | Stmt.Repeat (s, e) -> let initlbl, p = p#incr in
          let p, body = l_compile p s in
          p, [LABEL initlbl] @ body @ (expr e) @ [CJMP ("z", initlbl)]
  in let _, code = l_compile labels stmt in code