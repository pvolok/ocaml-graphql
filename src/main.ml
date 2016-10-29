open Graphql_schema
open Graphql_ast

module T = Graphql_schema.Type

let schema_of_string src =
  let lexbuf = Lexing.from_string src in
  let ast = Graphql_parser.doc Graphql_lexer.token lexbuf in
  Graphql_conv.schema_from_ast ast

let () =
  let schema = schema_of_string "scalar Int type User { name: String id: ID! }" in
  SMap.iter (fun name t ->
    match t with
    | T.Scalar name -> print_endline ("scalar " ^ name)
    | T.Obj (name, fields, _) ->
        print_endline ("type " ^ name);
        SMap.iter (fun name _ -> print_endline ("  " ^ name)) fields
    | _ -> ()
  ) schema.type_map;
