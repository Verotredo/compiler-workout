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
    let rec eval cfg stmt =
       let (st, i, o) = cfg in
      match stmt with
      | Read x -> (Expr.update x (List.hd i) st, List.tl i, o)
      | Write e -> (st, i, o @ [Expr.eval st e])
      | Assign (x, e) -> (Expr.update x (Expr.eval st e) st, i, o)
      | Seq (s1, s2) -> eval (eval cfg s1) s2
      | Skip -> cfg
      | If (e, s1, s2) -> eval cfg (if Expr.eval st e != 0 then s1 else s2)
      | While (e, s) ->
        if Expr.eval st e != 0 then eval (eval cfg s) stmt else cfg
      | RepeatUntil (e, s) ->
        let ((st', _, _) as cfg') = eval cfg s in
        if Expr.eval st' e = 0 then eval cfg' stmt else cfg'
    (* Statement parser *)        
    ostap (
      parse  : seq | stmt;
      stmt   : read | write | assign | skip | if' | while' | for' | repeat;
      read   : %"read" -"(" x:IDENT -")" { Read x };
      write  : %"write" -"(" e:!(Expr.parse) -")" { Write e };
      assign : x:IDENT -":=" e:!(Expr.parse) { Assign (x, e) };
      seq    : s1:stmt -";" s2:parse { Seq(s1, s2) };
      skip   : %"skip" { Skip };
      if'    : %"if" e:!(Expr.parse)
               %"then" s1:parse
                 elifs :(%"elif" !(Expr.parse) %"then" parse)*
                 else' :(%"else" parse)? %"fi"
                   {
                     let else'' = match else' with
                       | Some t -> t
                       | None -> Skip
                     in
                     let else''' = List.fold_right (fun (e', t') t -> If (e', t', t)) elifs else'' in
                     If (e, s1, else''')
                   };
      while' : %"while" e:!(Expr.parse) %"do" s:parse %"od" { While (e, s) };
      for'   : %"for" s1:parse "," e:!(Expr.parse) "," s2:parse %"do" s3:parse %"od" { Seq (s1, While (e, Seq (s3, s2))) };
      repeat : %"repeat" s:parse %"until" e:!(Expr.parse) { RepeatUntil (e, s) }
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
