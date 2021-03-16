open Type

type token =
| INT of int
| VARIABLE of string
| NAME of string
| LEFT_PAREN | RIGHT_PAREN
| LEFT_BRACKET | RIGHT_BRACKET
| PIPE | DOT | COMMA | COLON_HYPHEN
| PLUS | MINUS | MULT | DIV
| TERM_EQ | TERM_INEQ | IS | TERM_UNIFY | TERM_NOT_UNIFY
| TERM_VAR | TERM_NOT_VAR | TERM_INTEGER | TERM_NOT_INTEGER
| ARITH_EQ | ARITH_INEQ | ARITH_LESS | ARITH_GREATER | ARITH_GEQ | ARITH_LEQ
| EOF

let print = function
  | INT i -> Printf.printf "INT %d\n" i
  | VARIABLE var -> Printf.printf "VARIABLE %s\n" var
  | NAME name -> Printf.printf "NAME %s\n" name
  | LEFT_PAREN -> Printf.printf "LEFT_PAREN\n"
  | RIGHT_PAREN -> Printf.printf "RIGHT_PAREN\n"
  | LEFT_BRACKET -> Printf.printf "LEFT_BRACKET\n"
  | RIGHT_BRACKET -> Printf.printf "RIGHT_BRACKET\n"
  | PIPE -> Printf.printf "PIPE\n"
  | DOT -> Printf.printf "DOT\n"
  | COMMA -> Printf.printf "COMMA\n"
  | COLON_HYPHEN -> Printf.printf "COLON_HYPHEN\n"
  | PLUS -> Printf.printf "PLUS\n"
  | MINUS -> Printf.printf "MINUS\n"
  | MULT -> Printf.printf "MULT\n"
  | DIV -> Printf.printf "DIV\n"
  | TERM_EQ -> Printf.printf "TERM_EQ\n"
  | TERM_INEQ -> Printf.printf "TERM_INEQ\n"
  | IS -> Printf.printf "IS\n"
  | TERM_UNIFY -> Printf.printf "TERM_UNIFY\n"
  | TERM_NOT_UNIFY -> Printf.printf "TERM_NOT_UNIFY\n"
  | TERM_VAR -> Printf.printf "TERM_VAR\n"
  | TERM_NOT_VAR -> Printf.printf "TERM_NOT_VAR\n"
  | TERM_INTEGER -> Printf.printf "TERM_INTEGER\n"
  | TERM_NOT_INTEGER -> Printf.printf "TERM_NOT_INTEGER\n"
  | ARITH_EQ -> Printf.printf "ARITH_EQ\n"
  | ARITH_INEQ -> Printf.printf "ARITH_INEQ\n"
  | ARITH_LESS -> Printf.printf "ARITH_LESS\n"
  | ARITH_GREATER -> Printf.printf "ARITH_GREATER\n"
  | ARITH_GEQ -> Printf.printf "ARITH_GEQ\n"
  | ARITH_LEQ -> Printf.printf "ARITH_LEQ\n"
  | EOF -> Printf.printf "EOF\n"

let line_number = ref 1

let var_counter = ref min_int

let hashtable = Hashtbl.create 53

let reset () =
  line_number := 1;
  var_counter := min_int;
  Hashtbl.clear hashtable

let get_var_counter x =
  try
    if String.get x 0 = '_' then raise Not_found;
    Hashtbl.find hashtable x
  with Not_found ->
    begin
      let v = !var_counter in
      Hashtbl.add hashtable x v;
      incr var_counter;
      v
    end

exception Parse_error of string

let error msg = raise (Parse_error (msg ^ " at line " ^ string_of_int !line_number))

let init_tokens get_token =
  let nxt = ref (get_token ()) in
  let next () =
    let res = !nxt in
    nxt := get_token ();
    res
  in
  let peek () =
    !nxt
  in
  let junk () =
    nxt := get_token ()
  in
  next, peek, junk

let rec program get_token = clause_list (init_tokens get_token)

and query get_token = predicate_list (init_tokens get_token)

and clause_list ((next, peek, junk) as tokens) =
  let c = clause tokens in
  if peek () = EOF then [c]
  else c :: clause_list tokens

and clause ((next, peek, junk) as tokens) =
  let p = predicate tokens in
  match p with
  | BuiltinPredicate _ -> error "no builtin predicate in head of clause"
  | _ -> ();
  let l =
    match next () with
    | DOT -> []
    | COLON_HYPHEN -> predicate_list tokens
    | _ -> error "head of clause is followed by '.' or ':-'"
  in
  Clause(p, l)

and predicate ((next, peek, junk) as tokens) = failwith "TO DO"

and predicate_list ((next, peek, junk) as tokens) = failwith "TO DO"
















