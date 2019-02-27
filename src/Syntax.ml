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
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let of_int i = if i == 0 then false else true
    let to_int b = if b then 1 else 0
    let rec eval s e =
        match e with
        | Const x -> x
        | Var x -> s x
        | Binop (op, x, y) ->
            let l = eval s x in
            let r = eval s y in 
            match op with
            | "+" -> l + r
            | "-" -> l - r
            | "*" -> l * r
            | "/" -> l / r
            | "%" -> l mod r
            | "!!" -> to_int(of_int(l) || of_int(r))
            | "&&" -> to_int(of_int(l) && of_int(r))
            | "==" -> to_int(l == r)
            | "!=" -> to_int(l != r)
            | "<" -> to_int(l < r)
            | "<=" -> to_int(l <= r)
            | ">" -> to_int(l > r)
            | ">=" -> to_int(l >= r)
            | _ -> failwith(Printf.sprintf "Undefined expression")

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
    let rec eval cfg st =
        let (s, i, o) = cfg in
        match st with
        | Read    x       -> (Expr.update x (List.hd i) s, List.tl i, o)
        | Write   e       -> (s, i, o @ [Expr.eval s e])
        | Assign (v, e)   -> (Expr.update v (Expr.eval s e) cfg)
        | Seq    (e1, e2) ->
            let stmt = eval cfg e1
            in eval stmt e2
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
