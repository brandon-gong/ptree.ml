open Ptreedata

(* This module contains code for converting ptrees into string representation
   in file formats such as INI, XML, and JSON. I've separated the code for each
   into different modules for organization, and some helper functions that are
   shared across all of them are declared globally. This module only exports a
   few functions, i.e. some string validation functions and of course the
   ini_of_ptree, json_of_ptree, and xml_of_ptree functions. *)


(********************** Helper / utility functions **********************)

(* When the user calls one of our pretty printer functions, the peditor they
   provide might be anywhere in the tree. We want to print out the whole tree,
   so we cd to the root and then get out the ptree data from the peditor. *)
let get_whole_data pe = (root pe).under

(* Given a list of string,string pairs, where the first element in the pair
   is the element to search for, and the second is what to replace it with,
   this function will make all substitutions in a string. This is very useful
   for escaping strings; different file formats will require different chars to
   be escaped in different ways. *)
let substitutions lst str =
  let replace_all s (search, replace) =
    Str.global_replace (Str.regexp_string search) replace s in
    List.fold_left replace_all str lst


(* Two predicates useful for filtering out Trees and not-Trees; i.e. INI format
   handles trees separately. *)
let is_tree (_, v) = match v with
  | Tree _ -> true
  | _ -> false
let not_tree x = not @@ is_tree x


(* This function repeats the string s n times. This is used by JSON and XML
   pprinters to generate the right indentation level for formatting purposes. *)
let repeat n s = List.init n (fun _ -> s) |> String.concat ""

(* Often keys cannot contain certain reserved characters, so this function makes
   it easy to check if a string contains anything from a list of illegal char *)
let contains_any s clst = String.exists (fun c -> List.mem c clst) s


(*************************** Pretty-printers **************************)

module IniPrinter =
struct

  (* INI file format doesn't have a formal spec, but it makes sense to escape
     at least invisible characters, the dquote, and backslash. *)
  let escape = substitutions [
    "\n", "\\n";
    "\r", "\\r";
    "\t", "\\t";
    "\\", "\\\\";
    "\"", "\\\"";
    "\x0C", "\\f";
    "\b", "\\b";
  ]
  
  (* Converts a non-Tree ptree into a string. Most of these are fairly easy
     conversions; string and array are a little tricky, so we handle those in
     their own inner functions.

     The `force` parameter can be set to true to force strings to be wrapped in
     double quotes; if false, only some strings will need to be wrapped in
     double quotes in the INI format. *)
  let rec atom_to_str force = function
    | Int i -> Printf.sprintf "%Li" i
    | Float f -> Printf.sprintf "%f" f
    | String s -> handle_string force s
    | Boolean b -> if b then "true" else "false"
    | Null -> ""
    | Array a -> handle_array a
    | _ -> raise @@ Ptree_exception
        "INI format cannot handle nested subtrees or trees in arrays."
  
    and handle_string force s =
      let escaped = escape s in
      (* If the escaped string is different from the original, or if we
         want to `force` wrapping in quotes (i.e. in arrays), or if the string
         contains the equal sign, we wrap the string in quotes. Otherwise, we
         can omit it. *)
      if escaped <> s || force || contains_any s ['=' ; ';'] then
        "\"" ^ escaped ^ "\""
      else s
    
    (* For arrays, we simply render out all the inner elements (forcing strings
       to be wrapped in quotes so that if they contain commas, it doesn't
       confuse parsers) and join them together with commas. *)
    and handle_array a = a
      |> Array.to_list
      |> List.map (atom_to_str true)
      |> String.concat ", "
      |> fun middle -> "[" ^ middle ^ "]"
  
  (* This is very arbitrary, but just what we're running with for now (might
     change to something more accurate later): INI keys cannot contain square
     brackets, equal signs, or spaces, since those characters have other meaning
     in the INI format. *)
  let validate_ini_key k =
    if contains_any k ['[' ; ']' ; '='; ' '; ';'] then
      raise @@ Ptree_exception "INI key contained illegal character"
    else k
  
  (* We check to make sure k is a valid key, then concatenate k = v. *)
  let pprint_simple_rel (k, v) =
    (validate_ini_key k) ^ " = " ^ (atom_to_str false v)

  let pprint_simple_rels lst = lst
    |> List.map pprint_simple_rel
    |> String.concat "\n"

  (* Pretty-printing a subtree is very similar to just printing simple rels,
     but we also put the key as a [header] above the relationships; we validate
     the header to make sure it's ok. *)
  let pprint_subtree (k, t) =
    let header = "[" ^ (validate_ini_key k) ^ "]\n" in
    header ^ (get_tree t |> StrDict.bindings |> pprint_simple_rels)


  let ini_of_peditor pe =
    (* INI cannot handle depths greater than 2, so we throw exception if the
       user tries to do that. *)
    if depth (root pe) > 2 then
      raise @@ Ptree_exception
        "Cannot convert a tree of depth > 2 to INI format"
    else
      (* Get all the key-value pairs out of the tree, *)
      let binds = pe |> get_whole_data |> get_tree |> StrDict.bindings in

      (* Pretty-print all the simple rels, i.e. those pairs that are not in
         subtrees (we want to list them first, at the top of the file, so they
         aren't mistakenly included under any subsections),*)
      let simples = binds |> List.filter not_tree |> pprint_simple_rels in

      (* Pretty-print all of the subtrees together, joined by \n\n so there's an
         empty line between each subtree,*)
      let subtrees = binds
        |> List.filter is_tree
        |> List.map pprint_subtree in

      (* Now stick everything together. Either simples or subtrees might be "",
         so we put them in a list, filter our all the useless things, and join
         everything together with two newlines (for a blank line between) *)
      simples :: subtrees
      |>List.filter (fun s -> s <> "")
      |> String.concat "\n\n"
      |> fun x -> x ^ "\n"

end


module JsonPrinter =
struct

  (* Default, format using two spaces as tab. *)
  let default_tab = "  "

  (* These aren't all of the valid escapes that JSON supports, but it's all
     that we need to output valid JSON at least. Also, strings in JSON will no
     matter what need to be surrounded by quotes, so we go ahead and surround
     here. *)
  let escape s = "\"" ^ (substitutions [
    "\n", "\\n";
    "\r", "\\r";
    "\t", "\\t";
    "\\", "\\\\";
    "\"", "\\\"";
    "\x0C", "\\f";
    "\b", "\\b";
  ] s) ^ "\""


  (* A block is just a piece of text, i.e. a key-value pair, a whole object, or
     just a single value. formatblocks will arrange all given blocks on the same
     tab level, separated by comma and newline. *)
  let formatblocks lst tab level =
    let sep = "\n" ^ repeat (level - 1) tab in
    let sep1 = sep ^ tab in (* sep1 is just one extra tab level from sep *)
    sep1 ^ String.concat ("," ^ sep1) lst ^ sep

  
  (* The rest of the serialization is fairly obvious; array and tree are a bit
     less trivial, so I've pulled it out into separate functions. `tab` is the
     tab string to use, and level is the number of times to indent using `tab`.
     *)
  let rec to_str tab level = function
    | Int i -> Printf.sprintf "%Li" i
    | Float f -> Printf.sprintf "%f" f
    | String s -> escape s
    | Boolean b -> if b then "true" else "false"
    | Null -> "null"
    | Array a -> array_to_str a tab level
    | Tree t -> tree_to_str t tab level

  and array_to_str a tab level =
    (* If the array is trivial, we don't want to add newlines between brackets,
       so we just do an extra check here *)
    if Array.length a = 0 then "[]"
    else
      Array.to_list a
      (* Convert each element in the array to a string *)
      |> List.map @@ to_str tab (level + 1)
      (* Format these strings together by placing on newline, separate by comma *)
      |> fun x -> formatblocks x tab level
      (* Finally, just wrap that whole result in square brackets *)
      |> fun x -> "[" ^ x ^ "]"
  
  and tree_to_str t tab level =
    (* Similar reasoning as above, we check if the dict is empty *)
    if (StrDict.cardinal t) = 0 then "{}"
    else
      let render_pair (a, b) = escape a ^ ": " ^ to_str tab (level + 1) b in
      StrDict.bindings t
      |> List.map render_pair
      |> fun x -> formatblocks x tab level
      |> fun x -> "{" ^ x ^ "}"
      
  
  (* Get the root ptree out of the peditor, call to_str on that whole thing, and
     then append the trailing newline *)
  let json_of_peditor_tab tab pe = (to_str tab 1 @@ get_whole_data pe) ^ "\n"

  (* This is equivalent to calling json_of_peditor_tab pe default_tab, just a
     convenient alias if people don't really care about the tab used. *)
  let json_of_peditor = json_of_peditor_tab default_tab
end


module XmlPrinter =
struct

  let default_tab = "  "

  (* The suffix to append to a list key. TODO allow user to pass a function to
     control the key names of children of list elements *)
  let li_suffix = ".item"

  (* The escapes here are markedly different from IniPrinter and JsonPrinter,
     since we care about different characters. Take care to substitute out
     &amp; first, otherwise &lt; will be turned into &amp;lt;. *)
  let escape = substitutions [
    "&", "&amp;";
    "<", "&lt;";
    ">", "&gt;";
  ]

  (* Again, this is definitely not down-to-spec, and more here as a placeholder.
     If needed, this can be expanded upon to actually match the spec, probably
     using some very long regex or convoluted logic *)
  let validate_xml_key k =
    if contains_any k ['<' ; '>' ; '/'; ' '] then
      raise @@ Ptree_exception "XML key contained illegal character"
    else k
  
  (* We do this often, just a function that makes opening and closing tags from
     a key name. We validate the key here while we're at it. *)
  let make_tags tab level k =
    let vk = validate_xml_key k and indent = repeat level tab in
    indent ^ "<" ^ vk ^ ">\n", "\n" ^ indent ^ "</" ^ vk ^ ">"


  (* Unlike in previous formats, the atoms here will be rendered on their own
     lines, so it's helpful to define an indent function to help us do that *)
  let rec to_str tab level tagname =
    let indent txt = (repeat level tab) ^ txt in function
    | Int i -> Printf.sprintf "%Li" i |> indent
    | Float f -> Printf.sprintf "%f" f |> indent
    | String s -> escape s |> indent
    | Boolean b -> if b then "true" else "false" |> indent
    | Null -> ""
    | Array a -> array_to_str a tab level tagname
    | Tree t -> tree_to_str t tab level
  
    and array_to_str a tab level tagname =
      (* children of the array will have the array's tagname + a suffix. TODO
         this should be a user-provided lambda that can transform the array
         tagname however they want *)
      let child_tag = tagname ^ li_suffix in
      let opening, closing = make_tags tab level child_tag in

      Array.to_list a
      (* Stringify the child element. *)
      |> List.map @@ to_str tab (level + 1) child_tag
      (* Wrap it with the child_tags. *)
      |> List.map (fun x -> opening ^ x ^ closing)
      (* Stick all of them together at the end. *)
      |> String.concat "\n"

    and tree_to_str t tab level =
      
      (* Helper function to render pair as <k>v</k>. Make opening and closing
         tags and then wrap those around the stringified value. *)
      let render_pair (k, v) =
        let opening, closing = make_tags tab level k in
        opening ^ (to_str tab (level + 1) k v) ^ closing in
      
      (* Get all bindings out of the map *)
      StrDict.bindings t
      (* Render each k,v using render_pair. *)
      |> List.map render_pair
      (* Stick everything together with newlines *)
      |> String.concat "\n"

  let xml_of_peditor_tab rootname tab pe =
    let opening, closing = make_tags tab 0 rootname in
    opening ^ (to_str tab 1 rootname (get_whole_data pe)) ^ closing ^ "\n"

  let xml_of_peditor rootname = xml_of_peditor_tab rootname default_tab

end

(* I'll just include all of the modules willy-nilly here and then use pprint.mli
   to only export the few that I want to export. *)

include IniPrinter
include JsonPrinter
include XmlPrinter
