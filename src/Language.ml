
let counter = ref 0;;

let next_var() = let result = "__repeat_variable__" ^ string_of_int !counter in
                     counter := !counter + 1;
                     result;;

(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
       

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end

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
    let rec eval state expr = match expr with
      | Var v -> State.eval state v
      | Const c -> c
      | Binop (op, expr1, expr2) ->
        let e1 = eval state expr1 in
        let e2 = eval state expr2 in
        let numericbool num_to_bool = if num_to_bool != 0 then true else false in
        let boolnumeric bool_to_num = if bool_to_num then 1 else 0 in
        match op with
        | "+" -> (e1 + e2)
        | "-" -> (e1 - e2)
        | "*" -> (e1 * e2)
        | "/" -> (e1 / e2)
        | "%" -> (e1 mod e2)
        | ">" -> boolnumeric (e1 > e2)
        | ">=" -> boolnumeric (e1 >= e2)
        | "<" -> boolnumeric (e1 < e2)
        | "<=" -> boolnumeric (e1 <= e2)
        | "==" -> boolnumeric (e1 == e2)
        | "!=" -> boolnumeric (e1 != e2)
        | "!!" -> boolnumeric (numericbool e1 || numericbool e2)
        | "&&" -> boolnumeric (numericbool e1 && numericbool e2)
        | _ -> failwith "Error!"
     

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
               ) 
              [|                
		        `Lefta, ["!!"];
		        `Lefta, ["&&"];
		        `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		        `Lefta, ["+" ; "-"];
		        `Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
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
        (* loop with a post-condition       *) | Repeat of Expr.t * t
    (* call a procedure                 *) | Call   of string * Expr.t list with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator
         val eval : config -> t -> config
       Takes a configuration and a statement, and returns another configuration
    *)

let rec eval env (state, input, output) st = 
      match st with
      | Assign (x, e) -> (State.update x (Expr.eval state e) state, input, output)
      | Read x -> 
        (match input with 
        | z::i -> (State.update x z state, i, output)
        | [] -> failwith "input empty")
      | Write e -> (state, input, output @ [(Expr.eval state e)])
      | Seq (left_st, right_st) -> (eval env (eval env (state, input, output) left_st) right_st)
      | Skip -> (state, input, output)
      | If (e, s1, s2) -> if Expr.eval state e != 0 then eval env (state, input, output) s1 else eval env (state, input, output) s2
      | While  (e, s) -> if Expr.eval state e != 0 then eval env (eval env (state, input, output) s) st else (state, input, output)
      | Repeat (e, s) -> 
        let (state', input', output') = eval env (state, input, output) s in
        if Expr.eval state' e == 0 then eval env (state', input', output') st else (state', input', output')
      | Call (f, e)  ->
        let args, locals, body = env#definition f
        in let rec zip = function
        | x::xs, y::ys -> (x, y) :: zip (xs, ys)
        | [], []       -> []
        in let assign_arg st1 (x, e) = State.update x (Expr.eval state e) st1
        in let withArgs = List.fold_left assign_arg (State.enter state @@ args @ locals) @@ zip (args, e)
        in let state', input, output = eval env (withArgs, input, output) body
        in State.leave state state', input, output
                                
    (* Statement parser *)
    ostap (
      parse: seq | stmt;
       stmt: read | write | assign | if_ | while_ | for_ | repeat_ | skip;
       read: "read" -"(" x:IDENT -")" { Read x };
       write: "write" -"(" e:!(Expr.parse) -")" { Write e };
       assign: x:IDENT -":=" e:!(Expr.parse) { Assign (x, e) };
       if_: "if" e:!(Expr.parse) "then" s:parse "fi" {If (e, s, Skip)} 
          | "if" e:!(Expr.parse) "then" s1:parse else_elif:else_or_elif "fi" {If (e, s1, else_elif)};
       else_or_elif: else_ | elif_;
       else_: "else" s:parse {s};
       elif_: "elif" e:!(Expr.parse) "then" s1:parse s2:else_or_elif {If (e, s1, s2)};
       while_: "while" e:!(Expr.parse) "do" s:parse "od" {While (e, s)};
       for_: "for" init:parse "," e:!(Expr.parse) "," s1:parse "do" s2:parse "od" {Seq (init, While (e, Seq(s2, s1)))};
       repeat_: "repeat" s:parse "until" e:!(Expr.parse) {Repeat (e, s)};
       skip: "skip" {Skip};
       seq: left_st:stmt -";" right_st:parse { Seq (left_st, right_st) }
    )
      
  end



(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse: "fun" name:IDENT "(" args:(IDENT)* ")" local:(%"local" (IDENT)*)? "{" body:!(Stmt.parse) "}"
      {
          let local = match local with
          | Some x -> x
          | _ -> [] in
          name, (args, local, body)
      }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))

