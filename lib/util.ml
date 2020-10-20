
include Types
open Rresult
open R.Infix
module Yaml = Yaml

let member key = function 
  | `O assoc -> Ok (List.assoc_opt key assoc)
  | v ->  match Yaml.to_string v with 
     | Ok s -> Error (`Msg "Expected an `O")
     | Error (`Msg m) -> Error (`Msg m)
