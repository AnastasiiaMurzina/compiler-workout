open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string
(* create an S-expression          *) | SEXP    of string * int
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool
(* drops the top element off       *) | DROP
(* duplicates the top element      *) | DUP
(* swaps two top elements          *) | SWAP
(* checks the tag of S-expression  *) | TAG     of string
(* enters a scope                  *) | ENTER   of string list
(* leaves a scope                  *) | LEAVE
with show
                                                   
(* The type for the stack machine program *)
type prg = insn list

let print_prg p = List.iter (fun i -> Printf.printf "%s\n" (show(insn) i)) p
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n

let checkCJump cond value = let value = Value.to_int value in match cond with
  "nz" -> value <> 0
| "z" -> value = 0
          
let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) = function
  | [] -> conf
  | JMP l :: _ -> eval env conf (env#labeled l)
  | CJMP (znz, l) :: prg' -> let x::stack' = stack in eval env (cstack, stack', c) (
   if checkCJump znz x then (env#labeled l) else prg')
  | CALL (f, n, p) :: prg' ->  if env#is_label f then eval env ((prg', st)::cstack, stack, c) (env#labeled f) else eval env (env#builtin conf f n p) prg'
  | (END)::_ | (RET _) :: _-> (match cstack with 
    | (p, st')::cstack' -> eval env (cstack', stack, (State.leave st st', i, o)) p
    | _ -> eval env conf [])
  | insn :: prg' ->  let c' =
   (match insn with
    | BINOP  op -> let y::x::stack' = stack in (cstack, (Value.of_int @@ Expr.to_func op (Value.to_int x) (Value.to_int y)) :: stack', c)
      | CONST i  -> (cstack, (Value.of_int i)::stack, c)
      | STRING s -> (cstack, (Value.of_string s)::stack, c)
      | SEXP (s, n) -> failwith "Not implemented"
      | LD x     -> (cstack, (State.eval st x) :: stack, c)
      | ST x     -> let z::stack' = stack in (cstack, stack', (State.update x z st, i, o))
      | STA (x, n) -> let v::is, stack' = split (n+1) stack in
                                 (cstack, stack', (Language.Stmt.update st x v (List.rev is), i, o))
      | LABEL _ -> conf
      | BEGIN (_, p, l) -> let enter_st = State.enter st (p @ l) in
                           let (st', stack') = List.fold_right (
                               fun p (st'', x::stack') -> (State.update p x st'', stack')  
                                ) p (enter_st, stack) in
                            (cstack, stack', (st', i, o))
      | DROP -> cstack, List.tl stack, c
      | DUP -> cstack, List.hd stack :: stack, c
      | SWAP -> let e1::e2::stack' = stack in cstack, e2::e1::stack', c
      | TAG t -> let e1::stack' = stack in cstack, (Value.of_int (match e1 with Value.Sexp (t', _) when t' = t -> 1 | _ -> 0))::stack', c
      | LEAVE -> cstack, stack, (State.drop st, i, o)
      | ENTER xs -> let vs, stack' = split (List.length xs) stack in
        let state' = List.fold_left2 (fun s x v -> State.bind x v s) State.undefined xs vs in
          cstack, stack', (State.push st state' xs, i, o)
      )
       in eval env c' prg'


(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (* print_prg p; *)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           Printf.printf "Builtin: %s\n";
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class env =
    object(self)
        val number = 0
        method next_label = "l" ^ string_of_int number, 
                            {<number = number + 1>}
end

let compile (defs, p) = 
  let label s = "L" ^ s in

  let rec call f args p =
    let args_code = List.concat @@ List.map expr args in
    args_code @ [CALL (label f, List.length args, p)]

  and pattern lfalse = function
  | Stmt.Pattern.Wildcard | Stmt.Pattern.Ident _ -> [DROP]
  | Stmt.Pattern.Sexp (t, ps) -> [DUP; TAG t; CJMP ("z", lfalse)] @
    (List.concat (List.mapi
      (fun cons pr -> [DUP; CONST cons; CALL (".elem", 2, false)] @ pattern lfalse pr) ps))

  and bindings p = let rec inner = function
    | Stmt.Pattern.Wildcard -> [DROP]
    | Stmt.Pattern.Ident _ -> [SWAP]
    | Stmt.Pattern.Sexp (_, ps) -> (List.flatten 
      (List.mapi (fun cons h -> [DUP; CONST cons; CALL (".elem", 2, false)] @ bindings h) ps)) @ [DROP]
    in inner p @ [ENTER (Stmt.Pattern.vars p)]

  and expr e = (match e with
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, p) -> List.concat (List.map expr p) @ [CALL (f, List.length p, false)]
  | Expr.String s         -> [STRING s]
  | Expr.Array arr      -> List.concat (List.map expr arr) @ [CALL ("$array", List.length arr, false)]
  | Expr.Elem (a, i)      -> expr a @ expr i @ [CALL ("$elem", 2, false)]
  | Expr.Length a         -> expr a @ [CALL ("$length", 1, false)]) in

  let rec compile_stmt l env stmt = (match stmt with
  | Stmt.Seq (s1, s2) -> let l', env' = env#next_label in
    let env', c' = compile_stmt l' env'   s1 in
    let env'', c'' = compile_stmt l  env' s2 in 
      env'', c' @ [LABEL l'] @ c''
  | Stmt.If (e, s1, s2)    -> let iflbl, env = env#next_label in
                let endlbl, env = env#next_label in
                let env', prg' = compile_stmt l env s1 in
                let env'', prg'' = compile_stmt l env s2 in
                (env'', expr e @ [CJMP ("z", iflbl)] @ 
                prg' @ [JMP endlbl; LABEL iflbl] @ 
                prg'' @ [LABEL endlbl])
  | Stmt.While (e, s) -> let flbl, env = env#next_label in
                let endlbl, env  = env#next_label in
                let (env', prg') = compile_stmt l env s in 
                (env', [LABEL flbl] @ expr e @ [CJMP ("z", endlbl)] @
                prg' @ [JMP flbl; LABEL endlbl])
  | Stmt.Repeat (s, e)     -> let flbl, env = env#next_label in
                let (env' , prg') = compile_stmt l env s in
                (env', [LABEL flbl] @ prg' @ expr e @ [CJMP ("z", flbl)])
  | Stmt.Case (e, [r, s]) ->
      let pc = pattern l r in
      let env', sc = compile_stmt l env (Stmt.Seq (s, Stmt.Leave)) in
        env', expr e @ pc @ bindings r @ sc
  | Stmt.Case (e, brs) ->
      let l', _, _, code = 
        List.fold_left 
          (fun (env', e', ind, code) (p, s) -> 
             let (res, env), jmp = if   ind  == (List.length brs - 1) 
              then (l, env'), []
              else env#next_label, [JMP l] in 
            let pc       = pattern res p in 
             let env', sc = compile_stmt l env (Stmt.Seq (s, Stmt.Leave)) in  
               (env', Some res, ind + 1, ((match e' with 
                  None   -> [] 
                  | Some l -> [LABEL l]) @ pc @ bindings p @ sc @ jmp) :: code)) 
        (env, None, 0, []) brs
      in l', expr e @ (List.flatten (List.rev code))
  | _ -> (env, match stmt with
    | Stmt.Assign (x, [], e) -> expr e @ [ST x]
    | Stmt.Assign (x, is, e) -> List.concat (List.map expr is) @ expr e @ [STA (x, List.length is)]
    | Stmt.Skip -> []
    | Stmt.Leave -> [LEAVE]
    | Stmt.Return None -> [RET false]
    | Stmt.Return Some x -> (expr x) @ [RET true]
    | Stmt.Call (f, p) -> List.concat (List.map expr p) @ [CALL (f, List.length p, true)]
  )) in

  let compile_def env (name, (args, locals, stmt)) =
    let lend, env = env#next_label in
    let env,  code = compile_stmt lend env stmt in
    env,
    [LABEL name; BEGIN (name, args, locals)] @
    code @ [LABEL lend; END]
  
  in
  let comp_func env def_code =
    List.fold_left
      (fun (env, code) (name, others) -> let env', code' = compile_def env (label name, others) in env', code'::code)
      (env, []) def_code
  in
  let lend, env = (new env)#next_label in
  let env', code = compile_stmt lend env p in
  let env'', def_code = comp_func env' defs in
  (code @ [LABEL lend]) @ [END] @ (List.concat def_code) 
