val member : string -> Yaml.value -> (Yaml.value, Rresult.R.msg) Result.result



let rec get_value name assoclist =
  let v = List.get_value name assoclist in
  match v with
  | `Null -> Error "No Value for this key"
  | `Bool b -> Ok (`Bool b)
  | `Float f -> Ok (`Float f)
  | `String s -> Ok (`String s)
  | `A v -> Ok (`A v)
  | `O al -> get_value al 
  | _ -> Error "Unknown key"

let member (name:string) (yl: Yaml.value) = 
  match yl with
  | `O y -> get_value y 
  | yl -> Error "Not a Yaml value" 



