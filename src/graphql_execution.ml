module Ast = Graphql_ast
module Data = Graphql_data
module Schema = Graphql_schema

let find_op query_ast =
  let rec find defs =
    match defs with
    | [] -> failwith "Operation not found in query"
    | (Ast.Definition.Operation op) :: _ -> op
    | _ :: defs -> find defs
  in
  find query_ast.Ast.Document.definitions

let rec resolve_type schema type_ obj selection_set =
  match type_ with
  | Schema.Type.Scalar _ -> Data.String (Obj.magic obj)
  | Schema.Type.Obj (_, fields, _) ->
      let selections = match selection_set with
        | Some {Ast.SelectionSet.selections} -> selections
        | None -> failwith "Object field must have a selection set"
      in
      let map = List.fold_left (fun map selection ->
        match selection with
        | Ast.Selection.Field {Ast.Field.name = (_, name); selectionSet; _} ->
            let { Schema.Field.
              type_;
              resolve;
              _;
            } = Schema.SMap.find name fields in
            let result = resolve obj in
            let type_ = match type_ with
              | Schema.Type.Named name -> Schema.type_def schema name
              | _ -> failwith "Only named types are supported"
            in
            let value = resolve_type schema type_ result selectionSet in
            Schema.SMap.add name value map
        | _ -> failwith "Only fields are supported in selection set"
      ) Schema.SMap.empty selections in
      Data.Object map
  | _ -> failwith "Only scalar and object types are supported"

let execute schema query_ast =
  let op = find_op query_ast in
  let op_type = Schema.type_def schema "Query" in
  resolve_type schema op_type () (Some op.Ast.OperationDef.selectionSet)
