open GT       
       
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
let rec eval cfg prog = List.fold_left eval_instr cfg prog
    and eval_instr st_cfg instr =
    let (stack, cfg) = st_cfg in
    let (s, i, o)    = cfg in
    match instr with
        | BINOP op -> (match stack with
            | x::y::stack_rest -> ((Expr.eval_op op y x)::stack_rest, cfg)
            | _                ->  failwith "Stack is empty on binop")
        | CONST n -> (n::stack, cfg)
        | READ    -> (match i with
            | h::rest        -> (h::stack, (s, rest, o))
            | _                -> failwith "Unexpected end of input")
        | WRITE -> (match stack with
            | sh::stack_rest    -> (stack_rest, (s, i, o @ [sh]))
            | _                -> failwith "Stack is empty on write")
        | LD x -> ((s x)::stack, cfg)
        | ST x -> (match stack with
            | sh::stack_rest    -> (stack_rest, (Expr.update x sh s, i, o))
            | _                -> failwith "Stack is empty on store")  

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile stmt = match stmt with
    | Read    x       -> [READ; ST x]
    | Write   e       -> (compile_expr e)@[WRITE]
    | Assign (x, e)   -> (compile_expr e)@[ST x]
    | Seq    (s1, s2) -> (compile s1)@(compile s2)
    and compile_expr e = match e with
        | Expr.Const  n         -> [CONST n]
        | Expr.Var    x         -> [LD x]
        | Expr.Binop (op, a, b) -> (compile_expr a)@(compile_expr b)@[BINOP op] 

