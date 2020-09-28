
let member name (yl: Yaml.value) = 
  match yl with
  | `Null -> Error "Null yaml value"
  | `Bool b -> Ok b
  | `Float f -> Ok f
  | `String s -> Ok s
  | `A [ y ] -> Ok [ y ]
  | `O assoc -> Ok assoc 
  | yl -> Error "Unknown object: " ^ unk


