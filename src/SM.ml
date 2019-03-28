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
let rec eval env (st, (s, i, o)) prog =
    match prog with
    | []            -> (st, (s, i, o))
    | BINOP op :: p ->
        let y :: x :: st1 = st in
        let res = Expr.eval s (Binop (op, Const x, Const y))
        in eval env (res :: st1, (s, i, o)) p
    | CONST c  :: p -> eval env (c :: st, (s, i, o)) p
    | READ     :: p -> eval env ((List.hd i) :: st, (s, List.tl i, o)) p
    | WRITE    :: p -> eval env (List.tl st, (s, i, o @ [List.hd st])) p
    | LD x     :: p -> eval env (s x :: st, (s, i, o)) p
    | ST x     :: p -> eval env (List.tl st, (Expr.update x (List.hd st) s, i, o)) p 
    | LABEL _ :: p -> eval env (st, (s, i, o)) p
    | JMP l ::p -> eval env (st, (s, i, o)) (env#labeled l)
    | CJMP (z, l) :: p ->
        if (z = "z" && (List.hd st) == 0 || z = "nz" && (List.hd st) != 0)
          then eval env (List.tl st, (s, i, o)) (env#labeled l) 
          else eval env (List.tl st, (s, i, o)) p
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

let rec compile_block stmt end_label =
  let rec expr = function
    | Expr.Var x -> [LD x]
    | Expr.Const n -> [CONST n]
    | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op] in
  match stmt with
    | Stmt.Seq (s1, s2) -> 
      let l_end1 = label_generator#generate in
      let prg1, used1 = compile_block s1 l_end1 in
      let prg2, used2 = compile_block s2 end_label in
      prg1 @ (if used1 then [LABEL l_end1] else []) @ prg2, used2
    | Stmt.Read x -> [READ; ST x], false
    | Stmt.Write e -> expr e @ [WRITE], false
    | Stmt.Assign (x, e) -> expr e @ [ST x], false
    | Stmt.Skip -> [], false
    | Stmt.If (e, s1, s2) ->
      let l_else = label_generator#generate in
      let if_prg, used1 = compile_block s1 end_label in
      let else_prg, used2 = compile_block s2 end_label in
      expr e @ [CJMP ("z", l_else)] @ if_prg @ [JMP end_label] @ [LABEL l_else] @ else_prg @ [JMP end_label], true
    | Stmt.While (e, s) ->
      let l_cond = label_generator#generate in
      let l_loop = label_generator#generate in
      let (loop_prg, _) = compile_block s l_cond in
      [JMP l_cond; LABEL l_loop] @ loop_prg @ [LABEL l_cond] @ expr e @ [CJMP ("nz", l_loop)], false
    | Stmt.Repeat (s, e) ->
        let l_repeat = label_generator#generate in
        let repeat_prg = compile s in
        [LABEL l_repeat] @ repeat_prg @ expr e @ [CJMP ("z", l_repeat)], false
and compile stmt =
  let end_label = label_generator#generate in
  let prg, used = compile_block stmt end_label in
  prg @ (if used then [LABEL end_label] else [])