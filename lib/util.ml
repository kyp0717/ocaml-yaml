
open Rresult

(* let member key = function *) 
(*   | `O assoc -> Ok (List.assoc_opt key assoc) *)
(*   | v ->  match to_string v with *) 
(*      | Ok s -> Error (`Msg "Expected an `O") *)
(*      | Error (`Msg m) -> Error (`Msg m) *)


let member key = function 
  | `O assoc -> Ok (List.assoc_opt key assoc)
  | _ -> Error (`Msg "Not an `0")
