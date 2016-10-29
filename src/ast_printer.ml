open Graphql_ast
open Yojson

let position_to_json pos = `List [`Int pos.Loc.line; `Int pos.Loc.column]

let loc_to_json loc =
  `Assoc [
    ("start", position_to_json loc.Loc.start);
    ("end", position_to_json loc.Loc._end);
  ]

let kind_row kind = ("kind", `String kind)
let loc_row loc = ("loc", loc_to_json loc)

let optional opt fn =
  match opt with
  | None -> `Null
  | Some x -> fn x

let name_to_json (loc, name) = Name.(
  `Assoc [
    ("kind", `String "Name");
    ("value", `String name);
    ("loc", loc_to_json loc);
  ]
)

let rec type_to_json type_ = Type.(
  let fields = match type_ with
    | Named name -> [
        ("variant", `String "Named");
        ("name", name_to_json name);
      ]
    | List (loc, type_) -> [
        ("variant", `String "List");
        ("type", type_to_json type_);
        ("loc", loc_to_json loc);
      ]
    | NonNull (loc, type_) -> [
        ("variant", `String "NonNull");
        ("type", type_to_json type_);
        ("loc", loc_to_json loc);
      ]
  in
  `Assoc (("kind", `String "Type") :: fields)
)

let rec value_to_json value = Value.(
  let fields = match value with
    | Variable (loc, name) -> [
        ("variant", `String "Variable");
        ("name", name_to_json name);
        loc_row loc;
      ]
    | IntValue (loc, value) -> [
        ("variant", `String "IntValue");
        ("value", `String value);
        loc_row loc;
      ]
    | FloatValue (loc, value) -> [
        ("variant", `String "FloatValue");
        ("value", `String value);
        loc_row loc;
      ]
    | StringValue (loc, value) -> [
        ("variant", `String "StringValue");
        ("value", `String value);
        loc_row loc;
      ]
    | BooleanValue (loc, value) -> [
        ("variant", `String "BooleanValue");
        ("value", `Bool value);
        loc_row loc;
      ]
    | EnumValue (loc, value) -> [
        ("variant", `String "EnumValue");
        ("value", `String value);
        loc_row loc;
      ]
    | ListValue (loc, values) -> [
        ("variant", `String "ListValue");
        ("values", `List (List.map value_to_json values));
        loc_row loc;
      ]
    | ObjectValue obj_val -> obj_value_to_json obj_val
  in
  `Assoc ((kind_row "Value") :: fields)
)

and obj_value_to_json obj = ObjectValue.(
  [
    ("variant", `String "ObjectValue");
    ("fields", `List (List.map obj_field_to_json obj.fields));
    loc_row obj.loc;
  ]
)

and obj_field_to_json field = ObjectField.(
  `Assoc [
    kind_row "ObjectField";
    ("name", name_to_json field.name);
    ("value", value_to_json field.value);
    loc_row field.loc;
  ]
)

let argument_to_json arg = Argument.(
  `Assoc [
    kind_row "Argument";
    ("name", name_to_json arg.name);
    ("value", value_to_json arg.value);
    loc_row arg.loc;
  ]
)

let arguments_to_json args = `List (List.map argument_to_json args)

let directive_to_json directive = Directive.(
  `Assoc [
    kind_row "Directive";
    ("name", name_to_json directive.name);
    ("arguments", optional directive.arguments arguments_to_json);
    loc_row directive.loc;
  ]
)

let directives_to_json directives =
  match directives with
  | None -> `Null
  | Some dirs -> `List (List.map directive_to_json dirs)

let directives_row directives = ("directives", directives_to_json directives)

let operation op = OperationType.(
  match op with
  | Query -> "query"
  | Mutation -> "mutation"
  | Subscription -> "subscription"
)

let rec selection_to_json selection = Selection.(
  match selection with
  | Field field -> Field.(
      `Assoc [
        kind_row "Field";
        ("alias", optional field.alias name_to_json);
        ("name", name_to_json field.name);
        ("args", optional field.args arguments_to_json);
        ("selectionSet", optional field.selectionSet selection_set_to_json);
        directives_row field.directives;
      ]
    )
  | FragmentSpread spread -> FragmentSpread.(
      `Assoc [
        kind_row "FragmentSpread";
        ("name", name_to_json spread.name);
        directives_row spread.directives;
      ]
    )
  | InlineFragment frag -> InlineFragment.(
      `Assoc [
        kind_row "InlineFragment";
        ("typeCondition", optional frag.typeCondition name_to_json);
        ("selectionSet", selection_set_to_json frag.selectionSet);
        directives_row frag.directives;
      ]
    )
)

and selection_set_to_json ss = SelectionSet.(
  `Assoc [
    kind_row "SelecitonSet";
    ("selections", `List (List.map selection_to_json ss.selections));
  ]
)

let operation_def_to_json op = OperationDef.(
  `Assoc [
    kind_row "OperationDef";
    ("operation", `String (operation op.operation));
    ("name", optional op.name name_to_json);
    ("selectionSet", selection_set_to_json op.selectionSet);
    directives_row op.directives;
  ]
)

let frag_def_to_json frag = FragmentDef.(
  `Assoc [
    kind_row "FragmentDef";
    ("name", optional frag.name name_to_json);
    ("typeCondition", name_to_json frag.typeCondition);
    ("selectionSet", selection_set_to_json frag.selectionSet);
    directives_row frag.directives;
  ]
)

let input_value_def_to_json arg = InputValueDef.(
  `Assoc [
    kind_row "InputValueDef";
    ("name", name_to_json arg.name);
    ("type", type_to_json arg.type_);
    ("defaultValue", optional arg.defaultValue value_to_json);
    directives_row arg.directives;
    loc_row arg.loc;
  ]
)

let field_def_to_json field = FieldDef.(
  `Assoc [
    ("kind", `String "FieldDef");
    ("name", name_to_json field.name);
    ("type", type_to_json field.type_);
    ("args", `List (List.map input_value_def_to_json field.args));
    directives_row field.directives;
  ]
)

let scalar_def_to_json scalar = ScalarTypeDef.(
  `Assoc [
    kind_row "ScalarTypeDef";
    ("name", name_to_json scalar.name);
    directives_row scalar.directives;
  ]
)

let object_def_to_json obj = ObjectTypeDef.(
  `Assoc [
    ("kind", `String "ObjectTypeDef");
    ("name", name_to_json obj.name);
    ("fields", `List (List.map field_def_to_json obj.fields));
    ("interfaces", `List (List.map name_to_json obj.interfaces));
    directives_row obj.directives;
  ]
)

let interface_def_to_json interface = InterfaceTypeDef.(
  `Assoc [
    kind_row "InterfaceTypeDef";
    ("name", name_to_json interface.name);
    ("fields", `List (List.map field_def_to_json interface.fields));
    directives_row interface.directives;
  ]
)

let union_def_to_json union = UnionTypeDef.(
  `Assoc [
    kind_row "UnionTypeDef";
    ("name", name_to_json union.name);
    ("types", `List (List.map name_to_json union.types));
    directives_row union.directives;
  ]
)

let enum_value_def_to_json value = EnumValueDef.(
  `Assoc [
    kind_row "EnumValueDef";
    ("name", name_to_json value.name);
    directives_row value.directives;
    loc_row value.loc;
  ]
)

let enum_def_to_json enum = EnumTypeDef.(
  `Assoc [
    kind_row "EnumTypeDef";
    ("name", name_to_json enum.name);
    ("values", `List (List.map enum_value_def_to_json enum.values));
    directives_row enum.directives;
  ]
)

let input_object_def_to_json obj = InputObjectTypeDef.(
  `Assoc [
    kind_row "ObjectTypeDef";
    ("kind", `String "ObjectTypeDef");
    ("name", name_to_json obj.name);
    ("fields", `List (List.map input_value_def_to_json obj.fields));
    directives_row obj.directives;
  ]
)

let operation_type_to_json op_type = OperationType.(
  `Assoc [
    kind_row "OperationType";
    ("operation", `String (operation op_type.operation));
    ("type", name_to_json op_type.type_);
  ]
)

let schema_def_to_json schema = SchemaDef.(
  let op_types = schema.operationTypes in
  `Assoc [
    kind_row "SchemaDef";
    ("operationTypes", `List (List.map operation_type_to_json op_types));
    directives_row schema.directives;
  ]
)

let ast_to_json ast =
  let defs = ast.Document.definitions |> List.map (fun def ->
    let open Definition in
    match def with
    | Operation op -> operation_def_to_json op
    | Fragment frag -> frag_def_to_json frag
    | ScalarType scalar -> scalar_def_to_json scalar
    | ObjectType obj -> object_def_to_json obj
    | InterfaceType interface -> interface_def_to_json interface
    | UnionType union -> union_def_to_json union
    | EnumType enum -> enum_def_to_json enum
    | InputObjectType obj -> input_object_def_to_json obj
    | Schema schema -> schema_def_to_json schema
    | _ -> `Null
  ) in
  `List defs

let print ast =
  let json = ast_to_json ast in
  pretty_to_string json
