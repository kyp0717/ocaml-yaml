


module Member = ...

let member name (y: Yaml.value) = 
  let

  match y with
  | `O assoc -> OK assoc
  | _ -> Error ???


