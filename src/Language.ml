(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

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

    let parseBinop op = ostap(- $(op)), (fun x y -> Binop (op, x, y))
   (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (
      expr:
        !(Ostap.Util.expr
            (fun x -> x)
            (Array.map (fun (assoc, ops) -> assoc, List.map parseBinop ops)
                [|
                  `Lefta, ["!!"];
                  `Lefta, ["&&"];
                  `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
                  `Lefta, ["+"; "-"];
                  `Lefta, ["*"; "/"; "%"];
                |]
             )
            primary
        );
        primary: c: DECIMAL {Const c} | x: IDENT {Var x} | -"(" expr -")"
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t  with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config
       Takes a configuration and a statement, and returns another configuration
    *)
        let rec eval conf stmt =
      let (st, i, o) = conf in
      match stmt with
      | Read(var) -> (match i with
        | [] -> failwith (Printf.sprintf "Reached EOF")
        | head :: tail -> (Expr.update var head st, tail, o))
      | Write(expr) -> (st, i, o @ [Expr.eval st expr])
      | Assign(var, expr) -> (Expr.update var (Expr.eval st expr) st, i, o)
      | Seq(stmt1, stmt2) -> eval (eval conf stmt1) stmt2
      | Skip -> conf
      | If (cond, st1, st2) ->
          if (Expr.eval st cond) != 0 
            then (eval conf st1)
            else (eval conf st2)
      | While (cond, state) ->
          if (Expr.eval st cond) != 0
            then let updated = (eval conf state) in eval updated stmt
            else conf
      |  Repeat (state, cond) ->
          let updated = (eval conf state) in
          let (u_state, u_inp, u_out) = updated in 
          if (Expr.eval u_state cond) != 0
            then updated
            else eval updated stmt

    (* Statement parser *)
    ostap (
      parse: state:stmt";" rest:parse { Seq (state, rest) } | stmt;
      stmt: 
        "read" "(" name:IDENT ")" { Read name } 
        | "write" "(" expr:!(Expr.expr) ")" { Write expr } 
        | name:IDENT ":=" expr:!(Expr.expr) { Assign (name, expr) }
        | "skip" { Skip }
        | "if" cond:!(Expr.expr) "then" st1:parse st2:elsif { If (cond, st1, st2) }
        | "while" cond:!(Expr.expr) "do" st:parse "od" { While (cond, st) }
        | "repeat" state:parse "until" cond:!(Expr.expr) { Repeat (state, cond) }
        | "for" init:parse "," cond:!(Expr.expr) "," upd:parse "do" state:parse "od" { Seq (init, While (cond, Seq (state, upd))) };

      elsif:
        "fi" { Skip }
        | "else" state:parse "fi" { state }
        | "elif" cond:!(Expr.expr) "then" st1:parse st2:elsif{ If (cond, st1, st2) }
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
