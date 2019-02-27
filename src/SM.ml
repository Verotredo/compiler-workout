open GT       
open Syntax     
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
let rec eval st_cfg prog  =
    let (stack, cfg) = st_cfg in
    let (s, i, o)    = cfg in
    match prog with
        | BINOP op -> (match stack with
            | x::y::stack_rest -> ((Expr.eval s (Binop (op, Const x, Const y)))::stack_rest, cfg)
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
let rec compile_expr e = match e with
        | Syntax.Expr.Const  n         -> [CONST n]
        | Syntax.Expr.Var    x         -> [LD x]
        | Syntax.Expr.Binop (op, a, b) -> (compile_expr a)@(compile_expr b)@[BINOP op]

let rec compile st = match st with
    | Syntax.Stmt.Read    x       -> [READ; ST x]
    | Syntax.Stmt.Write   e       -> (compile_expr e)@[WRITE]
    | Syntax.Stmt.Assign (x, e)   -> (compile_expr e)@[ST x]
    | Syntax.Stmt.Seq    (s1, s2) -> (compile s1)@(compile s2)
