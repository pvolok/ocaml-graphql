module API = Graphql_api.Instance ()
open API

type user = {
  name: string;
};;

scalar_type "String";;

object_type
  ~name: "User"
  ~fields: [
    field
      ~name: "name"
      ~typ: (named "String")
      ~resolve: (fun user -> user.name);
  ]
  ;;

object_type
  ~name: "Query"
  ~fields: [
    field
      ~name: "me"
      ~typ: (named "User")
      ~resolve: (fun _ -> Obj.magic { name = "Pavel" });
  ]
  ;;

let schema = get_schema ()
