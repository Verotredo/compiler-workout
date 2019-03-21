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
      parse: expr;
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
    (* loop with a post-condition       *) | RepeatUntil of Expr.t * t  with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config
       Takes a configuration and a statement, and returns another configuration
    *)
    let reverse_cond cond = Expr.Binop ("==", cond, Expr.Const 0)
    let rec eval cfg stmt =
       let (st, i, o) = cfg in
      match stmt with
      | Read x -> (Expr.update x (List.hd i) st, List.tl i, o)
      | Write e -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e) -> (Expr.update x (Expr.eval st e) st, i, o)
      | Seq (s1, s2) -> eval (eval cfg s1) s2
      | Skip -> cfg
      | If (cond, thn, els) -> let cond_value = Expr.eval st cond in
                                 if cond_value <> 0 then
                                     eval (st, i, o) thn
                                 else
                                     eval (st, i, o) els
      | While (cond, body) -> let cond_value = Expr.eval st cond in
                                if cond_value == 0 then (st, i, o)
                                else
                                    let c' = eval (st, i, o) body in
                                    eval c' (While (cond, body))
      | RepeatUntil (body, cond) -> let c' = eval (st, i, o) body in
                                      eval c' (While (reverse_cond cond, body))
    (* Statement parser *)        
    ostap (
      stmt: "read" "(" x:IDENT ")" {Read x}
           | "write" "(" e:!(Expr.parse) ")" {Write e}
           | x:IDENT ":=" e:!(Expr.parse) {Assign (x, e)}
           | "if" condition:!(Expr.parse)
                "then" th:!(parse)
                elif:(%"elif" !(Expr.parse) %"then" !(parse))*
                els:(%"else" !(parse))?
                "fi"
                {
                    let else_body = match els with
                        | Some x -> x
                        | _ -> Skip
                    in
                    let t = List.fold_right (fun (cond, body) curr -> If (cond, body, curr)) elif else_body in
                    If (condition, th, t)
                }
            | "while" condition:!(Expr.parse) "do" body:!(parse) "od" { While (condition, body)}
            | "for" init:!(parse) "," cond:!(Expr.parse) "," step:!(parse) "do" body:!(parse) "od"
            {
                Seq(init, While(cond, Seq(body, step)))
            }
            | "repeat" body:!(parse) "until" cond:!(Expr.parse)
            { 
                RepeatUntil (body, cond)
            }
            | "skip" {Skip};
      parse: st1:stmt ";" st2:parse {Seq (st1, st2)} | stmt
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
