
let member key = function 
  | `O assoc -> Ok (List.assoc_opt key assoc)
  | v -> ( match Yaml.to_string v with 
     | Ok s -> Error (`Msg ("Expecting `O type. Got " ^ s " type"))
     | Error (`Msg m) -> Error (`Msg m))
