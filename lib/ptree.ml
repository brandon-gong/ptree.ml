include Ptreedata
include Parsing
include Pprint

(* Enum of types we currently know how to work with *)
type supported_types = JSON | XML | INI

(* Small helper functions to get contents of file as a string, and put string to
   file. *)
let get_txt path = In_channel.with_open_text path In_channel.input_all
let put_txt path txt =
  let oc = open_out path in
  Printf.fprintf oc "%s" txt;
  close_out oc; ()

let peditor_of_file_type path ftype =
  get_txt path |> match ftype with
  | JSON -> peditor_of_json
  | XML -> peditor_of_xml
  | INI -> peditor_of_ini

let get_file_type path =
  (* lowercase everything so we can match case-insensitively, split on ., and
     reverse the segments so the extension (if there is one) is the first
     element in the list. *)
  let split = path
    |> String.lowercase_ascii
    |> String.split_on_char '.'
    |> List.rev in
  match split with
  | "json"::_ -> JSON
  | "xml"::_ -> XML
  | "ini"::_ -> INI
  | _ -> raise @@ Ptree_exception "Unrecognized/missing file extension."

let peditor_of_file path = peditor_of_file_type path (get_file_type path)

let file_of_peditor_type path ftype pe =
  put_txt path @@ match ftype with
  | JSON -> json_of_peditor pe
  | XML -> xml_of_peditor pe "data"
  | INI -> ini_of_peditor pe

let file_of_peditor path = file_of_peditor_type path (get_file_type path)
