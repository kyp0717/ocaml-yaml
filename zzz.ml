

let files = [ "anchor.yml"; "cohttp.yml"; "linuxkit.yml"; "seq.yml"; "too_large.yml" ]

let dir f = Fpath.(v "yaml" / f)

let all_files = List.map dir ("bomb.yml"::files)

let all_simple_files = List.map dir files

let pp_comma ppf () = Format.fprintf ppf ",@"

let pp_fpath_list ppf fpaths =
  Format.fprintf ppf "[%a]" 
    Format.(pp_print_list ~pp_sep: pp_comma Fpath.pp) fpaths
