
open Rresult

let member key = function 
  | `O assoc -> Ok (List.assoc_opt key assoc)
  | _ -> Error (`Msg "Expecting a Yaml.value of `O")

let keys = function 
  | `O assoc -> Ok (List.map (fun (key, _) -> key) assoc)                                                     
  | _ -> Error (`Msg "Expecting a Yaml.value of `O")


(* let values yaml = *) 
(*   let map al = List.map (fun (_ , v) -> v) al in *)
(*   match yaml with *) 
(*   | `O assoc -> Ok (`A (map assoc)) *)                                                     
(*   | _ -> Error (`Msg "Expecting a Yaml.value of `O") *)


let values = function 
  | `A l -> Ok (List.map (fun (_, v) -> v) assoc) 
  | _ -> Error (`Msg "Expecting a Yaml.value of `O")


let to_string = function
  | `String a -> Ok a
  | _ -> Error (`Msg "Expecting a Yaml.value of `String")

let to_bool = function
  | `Bool a -> Ok a
  | _ -> Error (`Msg "Expecting a Yaml.value of `Bool")

let to_float = function
  | `Float a -> Ok a
  | _ -> Error (`Msg "Expecting a Yaml.value of `Float")

(* let rec map f =  function *)
(*   | `A (h::t) -> (f h)::(map t) *)
(*   | _ -> Error (`Msg "Expecting a Yaml.value of `A") *)


