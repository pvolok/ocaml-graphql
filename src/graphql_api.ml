open Utils
module S = Graphql_schema

module Instance () = struct
  let type_map = ref SMap.empty

  let add_type name type_ = type_map := SMap.add name type_ !type_map

  let scalar_type ~name = add_type name (S.Type.Scalar name)

  let object_type ~name ~fields =
    let fields = List.fold_left (fun map field ->
      SMap.add field.S.Field.name field map
    ) SMap.empty fields in
    let type_ = S.Type.Obj (name, fields, []) in
    add_type name type_

  let field ~name ~typ ~resolve =
    { S.Field.
      name;
      args = SMap.empty;
      type_ = typ;
      resolve = Obj.magic resolve;
    }

  let named name = S.Type.Named name

  let list type_ = S.Type.List type_

  let non_null type_ = S.Type.NonNull type_

  let get_schema () = { S.
    query_name = "Query";
    mutation_name = None;
    subscription_name = None;
    type_map = !type_map;
  }
end
