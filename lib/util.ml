
open Rresult

let member key = function 
  | `O assoc -> Ok (List.assoc_opt key assoc)
  | _ -> Error (`Msg "Expecting a Yaml.value of `O")


let keys = 
  let rec extract = function
    | [] -> []
    | (k,v)::t -> k::(extract t) in
  let get_keys = function 
    | `O assoc -> Ok (extract assoc)
    | _ -> Error (`Msg "Expecting a Yaml.value of `O") in
  get_keys 

