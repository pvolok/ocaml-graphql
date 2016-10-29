let () =
  let path = Sys.argv.(1) in
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  let ast = Graphql_parser.doc Graphql_lexer.token lexbuf in
  print_endline (Ast_printer.print ast)
