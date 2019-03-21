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
(* conditional jump                *) | CJMP  of string * string with show

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
let rec eval env scfg prog =
    let (st, cfg)      = scfg     in
    let (s, i, o) = cfg in
    match prog with
    | []            -> scfg
    | BINOP op :: p ->
        let y :: x :: st1 = st in
        let res = Expr.eval s (Binop (op, Const x, Const y))
        in eval (res :: st1, cfg) p
    | CONST c  :: p -> eval (c :: st, cfg) p
    | READ     :: p -> eval ((List.hd i) :: st, (s, List.tl i, o)) p
    | WRITE    :: p -> eval (List.tl st, (s, i, o @ [List.hd st])) p
    | LD x     :: p -> eval (s x :: st, cfg) p
    | ST x     :: p -> eval (List.tl st, (Expr.update x (List.hd st) s, i, o)) p 
    | LABEL _ :: p -> eval env scfg p
    | JMP l ::p -> eval env scfg (env#labeled l)
    | CJMP (m, label)::next ->
        let x::st1 = st in
        let goto = (env#labeled label) in
        let tg = if ((m="z") && (x == 0)|| x != 0 && m = "nz") then goto else next in
        eval env (st1, (s, i, o)) tg
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
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let label_generator =
  object
    val mutable counter = 0
    method generate =
      counter <- counter + 1;
      "l_" ^ string_of_int counter
  end

let rec compile_expr e = match e with
        | Expr.Const  n         -> [CONST n]
        | Expr.Var    x         -> [LD x]
        | Expr.Binop (op, a, b) -> (compile_expr a)@(compile_expr b)@[BINOP op]

let rec compile st = match st with
    | Stmt.Read    x       -> [READ; ST x]
    | Stmt.Write   e       -> (compile_expr e)@[WRITE]
    | Stmt.Assign (x, e)   -> (compile_expr e)@[ST x]
    | Stmt.Seq    (s1, s2) -> (compile s1)@(compile s2)
    | Stmt.Skip -> []
    | Stmt.If (e, s1, s2) ->
      let l_else = label_generator#generate in
      let l_fi = label_generator#generate in
      (compile_expr e) @ [CJMP ("z", l_else)] @ (compile s1) @ [JMP l_fi] @ [LABEL l_else] @ (compile s2) @ [LABEL l_fi]
    | Stmt.While (e, s) ->
      let l_expr = label_generator#generate in
      let l_od = label_generator#generate in
      [LABEL l_expr] @ (compile_expr e) @ [CJMP ("z", l_od)] @ (compile s) @ [JMP l_expr] @ [LABEL l_od]
    | Stmt.RepeatUntil (e, s) ->
      let l_repeat = label_generator#generate in
      [LABEL l_repeat] @ (compile s) @ (compile_expr e) @ [CJMP ("z", l_repeat)]
