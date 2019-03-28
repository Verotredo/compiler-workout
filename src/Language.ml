
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
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

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
        (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show

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
      | Call (name, args) ->
          let (arg_names, locals, body) = env#definition name in
          let args = List.combine arg_names (List.map (Expr.eval st) args) in
          let state = State.push_scope st (arg_names @ locals) in
          let fun_env_w_args = List.fold_left (fun st (name, value) -> State.update name value state) state args in
          let (new_st, input, output) = eval env (fun_env_w_args,input, output) body in
              (State.drop_scope new_st state, input, output)
    (* Statement parser *)
    ostap (
      parse: state:stmt ";" rest:parse { Seq (state, rest) } | stmt;
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

