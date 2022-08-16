open Ptreedata


(*************** Setting up: types and small helper functions **************)

type chars = char Seq.t
type label = string
type position = { line : int; col : int }

(* Keep track of position in input so we can report where errors happened easily *)
type input_state =
  { unconsumed : chars;
    pos : position;
  }

(* parse_result is either ok, in which we return what we parsed along with the
   remaining input to consume, or an error, in which we return what we were
   expecting (the label) and the position where the error occurred (position).*)
type 'a parse_result = ('a * input_state, label * position) result

(* Yes, I know parser is a reserved keyword in some old Caml systems, but
   it's fine :) *)
type 'a parser = 
  { run : input_state -> 'a parse_result;
    label : string
  }

(* Helper functions to increment line and col position *)
let next_col pos = { pos with col = pos.col + 1 }
let next_line pos = { line = pos.line + 1; col = 0 }


(* convert a string to input state by just making the string a char sequence and
   setting current position at (0, 0). *)
let make_input str =
  { unconsumed = String.to_seq str;
    pos = {line = 0; col = 0};
  }

let string_of_clst lst = lst |> List.to_seq |> String.of_seq
let ptree_of_clst lst = string_of_clst lst |> fun s -> String s

(* set_label simply runs the parser with the original label and, if an error
   occurs, swaps out that err's label with the one we want to set it to. *)
let set_label p lbl =
	let relabeled_run = fun input -> match p.run input with
	| Ok v -> Ok v (* If it runs successfully, great! Label doesn't matter *)
	| Error (_, p) -> Error (lbl, p) in (* Swap in new label for error *)
  { run = relabeled_run; label = lbl }

(* Also gonna be defining infix operators for a lot of different things as it's
   just more intuitive a lot of the time *)
let ( <?> ) = set_label

(* Get the next character from the input_state. Or returns None if no chars
   left. This also increments the position in the input state *)
let consume_char input =
  match input.unconsumed () with
  | Nil -> None, input
  | Cons (f, r) ->
    let new_pos = input.pos |>
      if f == '\n' then next_line else next_col in
    Some f, { unconsumed = r; pos = new_pos}

let make_parser f =
  { run = f; label = "UNSET LABEL" }


(********** The foundation: primitive parsers, basic combinators ***********)

(* The three most primitive parsers are return (sometimes called result, here
   called return as `result` is already being used by stdlib), zero, and item.
   *)


(* return takes any value v and returns a parser that does nothing to the input
   (doesn't consume any chars) and returns Ok with v as the parsed value. *)
let return v = make_parser @@ fun input -> Ok (v, input)

(* zero is a parser that, regardless of the input, fails. I use an obnoxious
   default label here, in the actual library this label should never show up 
   (i.e. higher-level parsers should always override it with set_label) *)
let zero = make_parser @@ fun input -> Error ("UNSET LABEL", input.pos)

(* item is a parser that just consumes the first char off of the input and
   returns it with Ok no matter what it is. item will only give Error if the
   input has no chars left to consume. *)
let item = make_parser @@ fun input ->
  match consume_char input with
  | None, _ -> Error ("Unexpected end of input", input.pos)
  | Some f, r -> Ok (f, r)

(* The classic monadic bind for parser types. *)
let bind p f = make_parser @@ fun input ->
  match p.run input with
  | Error x -> Error x
  | Ok (v, input') -> (f v).run input'
let ( >>= ) = bind

(* A simple parser built on top of item that, instead of returning Ok on
   any character regardless, first checks that the character satisfies a given
   predicate before returning Ok. If it fails the predicate, error. *)
let satisfies predicate =
  item >>= fun c -> if predicate c then return c else zero

(* Parse p1, and then immediately after parse p2. *)
let and_then p1 p2 =
  (p1 >>= fun v1 ->
  p2 >>= fun v2 -> return (v1, v2))
  
  (* Set the default label to be something intuitive *)
  <?> (Printf.sprintf "%s followed by %s" p1.label p2.label)

(* This operator is fairly easy to remember, cf. left_only and right_only infix
   operators. The sides of the >> that have asterisks are kept, the sides
   without asterisks are discarded. *)
let ( *>>* ) = and_then

(* This allows you to transform the output of a parser somewhat, e.g. if you
   have parser that returns a list of chars, you can (map string_of_clst p) to
   get back the same parser except it returns a string instead. *)
let map f p = (p >>= fun v -> return (f v)) <?> p.label
let ( |>> ) p f = map f p

(* In some cases we may want a parser to match something, but then we don't
   really care what it returns and we just want to return something else
   instead. That's what this operator does. *)
let ( >>% ) p x = p |>> fun _ -> x

(* Two parsers for keeping only the left or the right of an and_then sequence,
   respectively. *)
let left_only p1 p2 = (p1 *>>* p2) |>> fst (* Just keep first elt of tuple *)
let ( *>> ) = left_only
let right_only p1 p2 = (p1 *>>* p2) |>> snd
let ( >>* ) = right_only

(* Match _either_ p1 or p2. *)
let or_else p1 p2  = (make_parser @@ fun input ->
  match p1.run input with 
  | Ok v -> Ok v
  | Error _ -> p2.run input) (* TODO does it make sense to return p2's error only? *)
  <?> Printf.sprintf "%s or %s" p1.label p2.label
let ( <|> ) = or_else

(* Extending or_else to lists of parsers: match any parser out of a list of
   different choices. *)
let choice ps = List.fold_left or_else zero ps

(* Optionally match a parser. We need to map Option.some into p so that it
   returns an option type. *)
let opt p = (map Option.some p <|> return None)
  <?> Printf.sprintf "optionally %s" p.label

(* Match the same parser repeatedly, 0 or more times. Each match of the parser
   is returned as an element in a list, in the order that the matches
   occurred. *)
let many p =
  (* It's probably possible to build this off of and_then, but I just found this
     implementation cleaner and more intuitive. *)
  let rec multiple p = fun input ->
    match p.run input with
    | Error _ -> [], input
    | Ok (v, input') -> let vs, rem = multiple p input' in
      v::vs, rem in
  { run = (fun input -> Ok (multiple p input));
    label = Printf.sprintf "zero or more of %s" p.label;
  }

(* Just like `many`, except we enforce that there has to be at least one
   occurrence of p. *)
let many1 p = (p >>= (fun f -> many p >>= fun r -> return (f::r)))
  <?> Printf.sprintf "at least one of %s" p.label

(* In a similar vein with many, we often want to parse multiple occurrences of
   a certain element, delimited by commas or whitespace. sep_by and sep_by1
   allow us to do this, discarding those delimiters in the process. *)

(* It actually makes more sense to implement sep_by1 first, because p,p,p,p is
   not really a repeated pattern, so we instead parse p and then ,p ,p ,p. *)
let sep_by1 p sep = p *>>* many (sep >>* p) |>> fun (x, xs) -> x::xs
let sep_by p sep = sep_by1 p sep <|> return []


(*********** Building more useful parsers on top of the foundation **********)

(* Parse a specific character literal *)
let parse_char x = satisfies (fun c -> c == x) <?> Printf.sprintf "'%c'" x

(* any_of parses a character as long as it's in the provided list, and none_of
   parses any character as long as it's *not in* the list. *)
let any_of lst = lst |> List.map parse_char |> choice
let none_of lst = satisfies (fun c -> not (List.mem c lst))

(* I was using this a bunch, so just made it a small helper function. tests if
   test_ch is between a_ch and b_ch in terms of ascii code. (inclusive range) *)
let in_range a_ch b_ch test_ch =
	let a = Char.code a_ch
	and b = Char.code b_ch
	and c = Char.code test_ch in
	(a <= c) && (c <= b)

(* Common digit parsers *)
let digit = satisfies @@ in_range '0' '9' <?> "digit"
let hexdigit = satisfies (fun x ->
  in_range '0' '9' x
  || in_range 'a' 'f' x
  || in_range 'A' 'F' x) <?> "hex digit"

(* Common whitespace parsers *)
let whitespace = (satisfies @@ in_range '\x08' '\x0D' <|> parse_char ' ')
  <?> "whitespace character"
let spaces = many whitespace <?> "whitespace"
let padded p = (spaces >>* p *>> spaces) <?> p.label
let newline = parse_char '\n' <?> "newline"

(* Parse an exact string literal, like "true" or "null". Potential gotcha: this
   does not return the Ptree String variant, just a regular string. *)
let parse_string s =
  (* Parse one character of the string at a time, and at the end if everything
     parsed properly convert that list of characters back into a string. *)
  let rec parse_chars (chs: chars) = match chs () with
  | Nil -> return []
  | Cons (c, r) -> parse_char c >>= 
      (fun x -> parse_chars r >>=
      (fun xs -> return (x::xs))) in
  (parse_chars @@ String.to_seq s |>> string_of_clst)
    <?> Printf.sprintf "\"%s\"" s

(* Just using this for parse_int and parse_float *)
let num_sign = opt (any_of ['-'; '+']) <?> "number sign"

(* parse_int, parse_float, and parse_boolean are used in common for all of the
   different formats (WARNING: THIS MAY NOT BE DOWN TO SPEC) so I've pulled them
   out of the individual modules. *)
let parse_int =
  let hex_prefix = parse_string "0x" <|> parse_string "0X"
  and hex_number = many1 hexdigit
  and oct_prefix = parse_string "0o" <|> parse_string "0O"
  and oct_number = many1 @@ any_of ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7']
  and bin_prefix = parse_string "0b" <|> parse_string "0B"
  and bin_number = many1 @@ any_of ['0'; '1']
  and dec_prefix = parse_string "0u" <|> parse_string ""
  and dec_number = many1 digit in
  ((num_sign *>>* choice [
    hex_prefix *>>* hex_number;
    oct_prefix *>>* oct_number;
    bin_prefix *>>* bin_number;
    dec_prefix *>>* dec_number;
  ]) |>> fun (sign, (base, digits)) ->
    let sg = match sign with
      | Some '-' -> "-"
      | _ -> ""
    and num = string_of_clst digits in
    (* Once we've matched an integer with our parser, we just use OCaml's
       builtin int parsing to do the actual work. *)
    Int (Int64.of_string (sg ^ base ^ num))) <?> "integer"

let parse_float =
  (* '.1' is a valid float, and so is '1.', but not just '.'. So we have
     two alternatives, one is the float must have a fractional part, and one
     is the flaot must have an integer part. We enforce it using many1. *)
  let must_fpart = many digit *>> parse_char '.' *>>* many1 digit
  and must_ipart = many1 digit *>> parse_char '.' *>>* many digit
  and exponent = (parse_char 'e' <|> parse_char 'E') >>* many1 digit in
  ((num_sign *>>* (must_fpart <|> must_ipart) *>>* opt exponent)
  |>> fun ((sign, (ipart, fpart)), exp) ->
    let sg = match sign with
      | Some '-' -> "-"
      | _ -> ""
    and num = string_of_clst ipart ^ "." ^ string_of_clst fpart
    and e = match exp with
      | Some l -> "e" ^ string_of_clst l
      | None -> "" in
    Float (float_of_string @@ sg ^ num ^ e)) <?> "float"

let parse_boolean =
  let parse_true = parse_string "true" >>% Boolean true
  and parse_false = parse_string "false" >>% Boolean false in
  (parse_true <|> parse_false) <?> "boolean"

(* Two additional functions we'll be using in the file-specific modules.*)

(* I would've liked to have done this all purely, without mutation, but when we
   define recursive parsers (e.g. tree parsers that may contain other trees
   within them) we cannot simply use OCaml's let rec. Instead, what we'll have
   to do is "forward declare" the parser we want to create so that it already
   exists as a dummy, and then replace it later on once we've got the proper
   definition down. *)
let forward_declared_parser () =
  let parser_ref = ref zero in (* point it to zero at first since zero errors always *)
  (* Wrap the ref in a parser that simply dereferences and runs it *)
  let wrapped = make_parser @@ (fun input -> (!parser_ref).run input) in
  (* Now, when parser_ref is mutated, the behavior of wrapped will change as
     well. *)
  wrapped, parser_ref

(* It's a common case where we have a list of (string, ptree) pairs and we want
   to add all of them into a StrDict. *)
let put_dict lst dict =
  let add_to_dict d (k, v) = StrDict.add k v d in
  List.fold_left add_to_dict dict lst

(* In all of our file formats, certain special characters have meaning and
   thus must be escaped when present in strings. This function allows easily
   parsing those escapes and converting them back to their actual meaning. *)
let parse_escapes lst = lst
  |> List.map (fun (a,b) -> (parse_string b) >>% a)
  |> choice


(**************** Finally, parsers for each file format *****************)

module IniParser =
struct

  (* newlines actually matter for the INI format, so we need to override the
     global definitions of whitespace, spaces, and padded here to exclude
     newlines. *)
  let whitespace = (satisfies @@ (fun x -> x <> '\n' && in_range '\x08' '\x0D' x)
    <|> parse_char ' ')
    <?> "whitespace character"
  let spaces = many whitespace <?> "whitespace"
  let padded p = (spaces >>* p *>> spaces) <?> p.label
  
  (* A "key" is any of the [section] headers, or anything on the left of an
     equal sign in a key-value pair. It can't be empty. *)
  let parse_key =
    let disallowed_chars = [ '[' ; ']' ; '=' ; ' ' ; ';' ; '\n'] in
    (many1 @@ none_of disallowed_chars |>> string_of_clst)
      <?> "valid key"
  
  (* Comments start with a semicolon and run until the end of the line.*)
  let parse_comment =
    let semicolon = padded @@ parse_char ';' in
    (* following the semicolon, we ignore every char until newline *)
    (semicolon >>* (many @@ none_of ['\n']) |>> string_of_clst) <?> "comment"
  
  (* So the end of a line is optional spaces, comment, and then a mandatory
     newline. We discard everything but the newline. We also use many1 here
     to account for empty lines or lines containing only comments. *)
  let parse_eol =
    let single_eol = spaces >>* (opt parse_comment) >>* newline in
    many1 single_eol <?> "end of line(s)"

  (* A section header is between square brackets [like_this], is on its own
      line, and provides the name of the subsection containing the following
      key-value pairs. *)
  let parse_header =
    let left = parse_char '['
    and right = parse_char ']' in
    ((left >>* parse_key *>> right) *>> parse_eol)
      <?> "section header"
  
  (* INI allows for unquoted strings, so we'll allow it provided it doesn't
     contain characters that have special meaning*)
  let parse_unquoted_str =
    (many1 (none_of [';' ; '=' ; '"' ; '\n']) |>> ptree_of_clst)
    <?> "unquoted string"

  (* For all other strings, we parse escapes and expect quotes. *)
  let parse_quoted_str =
    let unescaped_char = none_of ['\\' ; '"']
    and escaped_char = parse_escapes [
        '\n', "\\n";
        '\r', "\\r";
        '\t', "\\t";
        '\\', "\\\\";
        '"', "\\\"";
        '\x0C', "\\f";
        '\b', "\\b"
      ]
    and quote = parse_char '"' in
    let charlst = many1 (escaped_char <|> unescaped_char) in
    ((quote >>* charlst *>> quote) |>> ptree_of_clst) <?> "quoted string"

  let parse_str = parse_unquoted_str <|> parse_quoted_str

  (* Null in ini is just a key and no associated value. *)
  let parse_null = (parse_string "" >>% Null) <?> "empty/null"

  (* Since arrays can contain other arrays, we have to forward-declare a parser
     for "any array element" before we can declare an array parser. *)
  let array_elt, array_elt_ref = forward_declared_parser ()

  let parse_array =
    let comma = padded (parse_char ',')
    and left = parse_char '[' *>> spaces
    and right = spaces >>* parse_char ']' in
    ((left >>* sep_by array_elt comma *>> right)
      |>> (fun lst -> Array (Array.of_list lst)))
      <?> "array"

  (* TAKE NOTE that the order of the parsers matters here. For example we must
     try to parse a float first before we parse an int, since floats are valid
     ints up until the decimal point. Most things must be parsed before str,
     since everything is also a valid string (e.g. true is a boolean, but can
     pass as a string). We check null last. *)
  let parse_atom = choice [
    parse_float *>> parse_eol;
    parse_int *>> parse_eol;
    parse_boolean *>> parse_eol;
    parse_array *>> parse_eol;
    parse_str *>> parse_eol;
    parse_null *>> parse_eol;
  ] <?> "atom"

  (* A key-value pair is a key, then an equal sign, then an atom (non-tree
     element) *)
  let parse_pair = (parse_key *>> padded (parse_char '=') *>>* parse_atom)
    <?> "key-value pair"

  (* A section is a header, followed by 0 or more key-value pairs. *)
  let parse_section = (parse_header *>>* many parse_pair) <?> "section"

  (* Every INI file starts with maybe some newlines and comments, then 0 or more
     pairs that aren't under a section header, and then 0 or more sections. *)
  let parse_ini = (opt parse_eol >>* many parse_pair *>>* many parse_section
    |>> fun (sectionless_rels, subsections) ->

      (* First put all the pairs that aren't under a header into our final
         dict *)
      let tree = put_dict sectionless_rels StrDict.empty in

      (* Then, for each section, put those pairs into separate sub-dicts *)
      List.map (fun (l, p) -> (l, put_dict p StrDict.empty)) subsections

      (* and then combine everything together. *)
      |> List.map (fun (l, p) -> (l, Tree p))
      |> fun x -> put_dict x tree
      |> fun x -> Tree x)
    <?> "INI"

  (* I know the double semicolon is considered bad style, but I'm honestly not
     sure how to mutate array_elt_ref at this point at the end of the module,
     and this happens to work. *)
  ;;

  (* Note order matters here, just like parse_atom. *)
  array_elt_ref := choice [
    parse_float;
    parse_int;
    parse_boolean;
    parse_quoted_str;
    parse_array;
    parse_null;
  ];
end

module JsonParser =
struct

  (* JSON is a bit easier to parse than INI because newlines don't matter, and
     we can nest trees infinitely. We also must have quoted strings. That being
     said, a lot of concepts are shared between this and the IniParser module,
     so comments may be a bit more sparse *)

  (* This is not quite down to spec. JavaScript also supports a \uXXXX escape
     for unicode characters. I'm not sure how to make it work here, but may
     fix later. *)
  let parse_str =
    let unescaped_char = none_of ['\\' ; '"']
    and escaped_char = parse_escapes [
        '\n', "\\n";
        '\r', "\\r";
        '\t', "\\t";
        '\\', "\\\\";
        '/', "\\/";
        '"', "\\\"";
        '\x0C', "\\f";
        '\b', "\\b"
      ]
    and quote = parse_char '"' in
    let chs = quote >>* (many (unescaped_char <|> escaped_char)) *>> quote in
    (chs |>> ptree_of_clst) <?> "string"
  
  let parse_null = (parse_string "null" >>% Null) <?> "null"
  
  (* Just like earlier, both our parse_array and parse_tree functions are
     circular, so we need to forward-declare them. *)
  let parse_json, parse_json_ref = forward_declared_parser ()

  let parse_array =
    let left = parse_char '[' *>> spaces
    and right = parse_char ']' *>> spaces
    and comma = padded (parse_char ',') in
    ((left >>* sep_by parse_json comma *>> right)
    |>> fun lst -> Array (Array.of_list lst)) <?> "array"

  (* Similar concept to the Ini module where we have a key-value pair parser
     that returns a (string * ptree), but we use a colon delimiter instead. *)
  let parse_pair =
    let colon = padded (parse_char ':') in
    ((parse_str |>> get_string) *>> colon *>>* parse_json)
      <?> "key-value pair"

  let parse_tree =
    let left = parse_char '{' *>> spaces
    and right = parse_char '}' *>> spaces
    and comma = padded (parse_char ',') in 
    (padded (left >>* (sep_by parse_pair comma) *>> right)
    |>> fun lst -> Tree (put_dict lst StrDict.empty)) <?> "JSON Object"

  ;;

  (* Order matters a bit less here, just float needs to be ahead of int. *)
  parse_json_ref := choice [
    parse_float *>> spaces;
    parse_int *>> spaces;
    parse_boolean *>> spaces;
    parse_null *>> spaces;
    parse_str *>> spaces;
    parse_array *>> spaces;
    parse_tree *>> spaces;
  ];

end

module XmlParser =
struct

  (* XML is a bit weird. Key-value pairs "look" the same as trees in that
     they are just tags that happen to contain only atoms instead of key-value
     pairs. Also, tags can have attributes <name attr="value"></name> or
    instead have their values nested within.
    
    Again, this parser is definitely not fully down-to-spec. *)

  let parse_key =
    let disallowed_chars = [ '<' ; '>' ; ' ' ; '/' ; '"' ] in
    (many1 (none_of disallowed_chars) |>> string_of_clst)
      <?> "tag name"


  (* Again, an "attribute" is a key-value-esque pair thats included in the
     opening tag of an XML element (see above comment for example). *)
  let parse_attr =
    let equal = padded (parse_char '=')
    and quote = parse_char '"' in
    let attr = (quote >>* many (none_of ['"']) *>> quote) |>> ptree_of_clst in
    (parse_key *>> equal *>>* attr) <?> "attribute"

  (* We use many1 here because, like INI, a blank field is counted as null and
     not the empty string literal "" *)
  let parse_str =
    let unescaped_char = none_of ['&' ; '<' ; '>']
      and escaped_char = parse_escapes [
        '&', "&amp;";
        '<', "&lt;";
        '>', "&gt;"
      ] in
    (many1 (unescaped_char <|> escaped_char) |>> string_of_clst
      |>> fun s -> String (String.trim s)) <?> "string"

  let parse_null = (parse_string "" >>% Null) <?> "null/empty"

  (* Unlike the other formats, I'm not going to think about arrays for now,
     since there's not a special construct for arrays (i.e. other formats use
     the square brackets, xml just uses more tags)*)
  let parse_atom = choice [
    parse_float;
    parse_int;
    parse_boolean;
    parse_str;
    parse_null;
  ] <?> "atom"

  let parse_elt, parse_elt_ref = forward_declared_parser ()

  (* Parsing matching opening and closing tags is a bit tricky - we need to
     first parse any valid key, then use bind to force parsing a matching
     closing tag. *)
  let parse_wrapped_tag p =
    let angle_wrap k = (parse_char '<') >>* k *>> (parse_char '>') in
    (angle_wrap parse_key) >>= (* parse <key> (opening tag) *)
    fun s -> (p >>= (* Parse the inner element *)
    fun r -> (angle_wrap (parse_char '/' >>* parse_string s) >>= (* parse closing tag *)
    fun _ -> return (s, r))) (* return (key, inner element) *)

  (* This is the equivalent of parse_pair in the JSON and INI parsers. *)
  let parse_simple = parse_wrapped_tag (padded parse_atom) <?> "key-value pair"

  (* Unfortunately, we can't use parse_wrapped_tag here for tree, since tree
     nodes can have attributes as well that parse_wrapped_tag wouldn't support. *)
  let parse_tree = 
    let angle_wrap k = (parse_char '<') >>* k *>> (parse_char '>')
    and parse_tag_and_attrs = padded parse_key *>>* sep_by parse_attr spaces in
    (* Parse the opening tag, to get back the tag name and key-val attribute
       pairs. *)
    ((angle_wrap parse_tag_and_attrs) >>=
    (* Parse all the inner elements. *)
    fun (k, attr) -> (padded (sep_by parse_elt spaces) >>=
    (* Parse the closing tag, enforcing it has the same name as the opening
       tag name. *)
    fun nodes -> (angle_wrap (parse_char '/' >>* parse_string k) >>=
    (* Put the attributes and parsed nodes into a Tree. *)
    fun _ ->
      let all_children = attr @ nodes
      and add_with_key d (k,v) = match StrDict.find_opt k d with
        | None -> StrDict.add k [v] d
        | Some lst -> StrDict.add k (v::lst) d in
      (* We first accumulate all elements with the same key name into lists *)
      List.fold_left add_with_key StrDict.empty all_children
      (* And then, if the list is more than 1 elt long, make it an Array, 
         otherwise unwrap it into the single ptree value *)
      |> StrDict.map (fun v -> match v with
        | [e] -> e
        | x -> Array (Array.of_list x))
      |> fun d -> return (k, Tree d)
      ))) <?> "tree element"

  (* Our overall XML parser is basically parse_tree, but we can discard the
     root tag name. *)
  let parse_xml = parse_elt |>> snd
  
  ;;
  
  parse_elt_ref := parse_simple <|> parse_tree
end

let make_opt f x = try Some (f x) with
  Ptree_exception _ -> None

(* Given a parser for a certain file type, try to parse it, and if it fails
   raise a Ptree_exception. We need to check that the parsing did not
   prematurely terminate, and we do so by checking for any unconsumed input. *)
let peditor_of_x p str = match p.run (make_input str) with
  | Ok (pt, r) -> (match fst (consume_char r) with
    | None -> peditor_of_ptree pt
    | Some _ -> raise @@ Ptree_exception
      (Printf.sprintf "Syntax error at line %d column %d" r.pos.line r.pos.col))
  | Error (l, p) -> raise @@ Ptree_exception
    (Printf.sprintf "Syntax error: %s\n at line %d column %d" l p.line p.col)


(********************* module exports *********************)

let peditor_of_ini = peditor_of_x IniParser.parse_ini
let peditor_of_json = peditor_of_x JsonParser.parse_json
let peditor_of_xml = peditor_of_x XmlParser.parse_xml

let peditor_of_ini_opt = make_opt peditor_of_ini
let peditor_of_json_opt = make_opt peditor_of_json
let peditor_of_xml_opt = make_opt peditor_of_xml
