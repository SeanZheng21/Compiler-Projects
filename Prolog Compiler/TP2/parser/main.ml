let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    s

let concat_files filenames =
  List.map load_file filenames |> String.concat ""

let main () =
  let filenames = ref [] in
  let specs = []
  and add_to_filenames = fun s -> filenames := s :: !filenames in
  Arg.parse specs add_to_filenames "Usage: prolog filename";
  let stream = concat_files !filenames |> Stream.of_string in
  let rules = Parser.program (fun () -> Lexer.get_token stream) in
  Ast.print_program rules

let () = main ()
