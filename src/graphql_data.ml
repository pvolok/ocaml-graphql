open Utils

type t =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | Object of t SMap.t
  | List of t list

let rec to_yojson data =
  match data with
  | String v -> `String v
  | Int v -> `Int v
  | Float v -> `Float v
  | Bool v -> `Bool v
  | Null -> `Null
  | Object map ->
      `Assoc (SMap.fold (fun k v acc -> (k, to_yojson v) :: acc) map [])
  | List lst -> `List (List.map to_yojson lst)

let to_json data = Yojson.pretty_to_string (to_yojson data)
