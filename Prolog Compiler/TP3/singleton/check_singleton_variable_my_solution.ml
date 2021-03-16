open Type

let rec get_variables_from_term (t: term) =
    let rec get_variables_from_functor termList = match termList with
    | head::tail -> (get_variables_from_term head) @ (get_variables_from_functor tail)
    | [] -> []
    in
    match t with 
    | Functor (_, termList) -> get_variables_from_functor termList
    | BuiltinFunctor (_, t1, t2) -> (get_variables_from_term t1)@(get_variables_from_term t2)
    | Integer(_) -> []
    | Variable (s, v1, v2) -> [s, v1, v2]
    | EmptyList -> []
    | Cons (t1, t2) -> (get_variables_from_term t1)@(get_variables_from_term t2)

let rec get_variables_from_builtin_predicate builtin_predicate = 
    match builtin_predicate with
    | Is (t1, t2)
    | ArithmeticEquality (t1, t2)
    | ArithmeticInequality  (t1, t2)
    | ArithmeticLess (t1, t2)
    | ArithmeticGreater (t1, t2)
    | ArithmeticLeq (t1, t2)
    | ArithmeticGeq (t1, t2)
    | TermEquality (t1, t2)
    | TermInequality (t1, t2)
    | TermUnify (t1, t2)
    | TermNotUnify (t1, t2) -> (get_variables_from_term t1) @ (get_variables_from_term t2)
    | TermVar (t)
    | TermNotVar (t)
    | TermInteger (t)
    | TermNotInteger (t) -> get_variables_from_term t


let get_variables_from_predicate (predicate : predicate) : (string * int * int) list = 
    let rec get_variables_from_term_list termList = match termList with
    | head::tail -> (get_variables_from_term head) @ (get_variables_from_term_list tail)
    | [] -> []
    in
    match predicate with
    | BuiltinPredicate bp -> get_variables_from_builtin_predicate bp
    | Predicate (_, termList) -> get_variables_from_term_list termList


let check_clause = function
  | Clause(predicate, predicate_list) ->
    let variables =
      predicate :: predicate_list
      |> List.map get_variables_from_predicate
      |> List.concat
      |> List.sort Pervasives.compare
    in
    let print_error (pos, x) =
      Printf.printf "singleton variable %s at line %d\n" x pos
    in
    let rec singleton_variable prev = function
      | [] -> []
      | (x, _, _) :: r when String.get x 0 = '_' -> singleton_variable None r
      | (x1, id1, pos1) :: (x2, id2, pos2) :: r when id1 = id2 -> singleton_variable (Some id1) r
      | (x, id, pos) :: r when prev = Some id -> singleton_variable prev r
      | (x, id, pos) :: r -> (pos, x) :: singleton_variable (Some id) r
    in
    singleton_variable None variables
    |> List.sort Pervasives.compare
    |> List.iter print_error

let check_program prog = List.iter check_clause prog
