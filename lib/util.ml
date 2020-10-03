
open Rresult


module type M = sig 
  type v
  type al
  type 'a res 
  val to_value : string -> al -> v
  val from_value : string -> v -> (v*string) res 
  val member : string -> v -> (v*string) res  
end 

module Member = struct
  type v = Yaml.value
  type al = (string*v) list
  type 'a res = (v,string) result
  let to_value n al = List.assoc n al 

end



module M : sig 
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



