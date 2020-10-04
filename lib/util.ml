
open Rresult


module Member : sig 
  type v
  type al
  type 'a res 
  val to_value : string -> al -> v
  val member : string -> v -> (v*string) res  
end = struct
  type v = Yaml.value
  type al = (string*v) list
  type 'a res = (v,string) result
  let to_value n al = List.assoc n al 
  let member n ym = 
    match ym with
    | `O al -> Ok (to_value n al)
    | ym -> Error "Not a Yaml Value"
end






