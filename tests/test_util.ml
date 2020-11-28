[@@@part "0"]


let yaml = Alcotest.testable Yaml.pp Yaml.equal

let pp_error ppf (`Msg x) = Format.pp_print_string ppf x

let error = Alcotest.testable pp_error ( = )

[@@@part "1"]

let test_member () =
  let ok_input = `O [ ("author", `String "Alice") ] in
  let ok_output = Ok (Some (`String "Alice") ) in
  let err_input = `String "Test_Error" in
  let err_output = Error (`Msg "Expecting a Yaml.value of `O") in
  Alcotest.(check (result (option yaml) error)) "success" ok_output (Yaml.Util.member "author" ok_input);
  Alcotest.(check (result (option yaml) error))  "fail" err_output (Yaml.Util.member "author" err_input)


let pp_comma ppf () = Format.fprintf ppf ",@"

let pp_print_keys ppf key_list= 
  Format.fprintf ppf "[%a]" 
      Format.(pp_print_list ~pp_sep:pp_comma pp_print_string) key_list

let keys_equal a b = (a=b)
let keys_compare = Alcotest.testable pp_print_keys keys_equal

let test_keys () =
  let ok_input = `O [ ("first", `String "John");("last", `String "Doe") ] in
  let ok_output = Ok ["first";"last"] in
  let err_input = `String "Test_Error" in
  let err_output = Error (`Msg "Expecting a Yaml.value of `O") in
  Alcotest.(check (result (keys_compare) error)) "success" ok_output (Yaml.Util.keys ok_input);
  Alcotest.(check (result (keys_compare) error))  "fail" err_output (Yaml.Util.keys err_input)

let pp_print_values ppf value_list= 
  Format.fprintf ppf "[%a]" 
      Format.(pp_print_list ~pp_sep:pp_comma Yaml.pp) value_list

let values_equal a b = (a=b)
let values_compare = Alcotest.testable pp_print_values values_equal

let test_to_list () =
  let ok_input = `O [ ("first", `String "John");("last", `String "Doe") ] in
  let ok_output = Ok  [ (`String "John") ; (`String "Doe") ]  in
  let err_input = `String "Test_Error" in
  let err_output = Error (`Msg "Expecting a Yaml.value of `A") in
  Alcotest.(check (result (values_compare) error)) "success" ok_output (Yaml.Util.to_list ok_input);
  Alcotest.(check (result (values_compare) error))  "fail" err_output (Yaml.Util.to_list err_input)

let pp_value_string  ppf (`String x) = Format.pp_print_string ppf x
let string_compare = Alcotest.testable pp_value_string ( = )

let test_to_string () =
  let ok_input = `String "John" in
  let ok_output =  Ok "John"  in
  let err_input = `Bool true in
  let err_output = Error (`Msg "Expecting a Yaml.value of `String") in
  Alcotest.(check (result (string_compare) error)) "success" ok_output (Yaml.Util.to_string ok_input);
  Alcotest.(check (result (string_compare) error))  "fail" err_output (Yaml.Util.to_string err_input)

[@@@part "2"]

