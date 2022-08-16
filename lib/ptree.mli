(** ptree.ml is a multipurpose data serialization / deserialization library
	 for OCaml, currently supporting INI, JSON, and XML file types. It models
	 the data as a file system that can be explored and edited using commands
	 reminiscent of the Unix terminal. *)

open Ptreedata

(** The exception type used throughout the library. Is generally thrown whenever
	 parsing fails, an invalid action is attempted in a peditor, or a peditor
	 cannot be written to a certain file format. *)
exception Ptree_exception of string

(** An immutable map from strings to any value, generated using the
		[Stdlib.Map.Make] functor. In this library, it is primarily used to map from
		key names to ptree values. *)
module StrDict = StrDict

(** A [ptree] (short for "property tree") is the core data structure of the
		library and represents a tree of properties. It can take on a variety of
		different leaf node types (integer, floating-point, strings, booleans, etc)
		or it can hold other ptree values in its Array and Tree variants. *)
type ptree =
  | Int of int64
  | Float of float
  | String of string
  | Boolean of bool
  | Null
  | Array of ptree array
  | Tree of ptree StrDict.t

(** The [peditor] (short for "property editor") is the structure that users will
		primarily interact with. It is essentially a pointer to a certain position
		in a ptree, and provides operations for navigating through and editing the
		ptree.

	  Note that the [under] and [above] attributes in the record object are not
	  intended for users to directly access or modify, and are for internal
	  bookkeeping purposes. Stick to using provided functions to interact with
	  peditor objects. *)
type peditor = {
  under : ptree;
  above : (string * peditor) option;
}

(** Get the integer value out of a Ptree data. Raises [Ptree_exception] if the
		given Ptree is not an [Int] variant. *)
val get_int : ptree -> int64

(** Get the float value out of a Ptree data. Raises [Ptree_exception] if the
		given Ptree is not a [Float] variant. *)
val get_float : ptree -> float

(** Get the string value out of a Ptree data. Raises [Ptree_exception] if the
		given Ptree is not a [String] variant. *)
val get_string : ptree -> string

(** Get the boolean value out of a Ptree data. Raises [Ptree_exception] if the
		given Ptree is not a [Boolean] variant. *)
val get_boolean : ptree -> bool

(** Get the array value out of a Ptree data. Raises [Ptree_exception] if the
		given Ptree is not an [Array] variant. *)
val get_array : ptree -> ptree array

(** Get the tree value out of a Ptree data. Raises [Ptree_exception] if the
		given Ptree is not a [Tree] variant. *)
val get_tree : ptree -> ptree StrDict.t

(** The "empty" [peditor] points to the root of an empty [ptree]. This can
		be used if trying to serialize from scratch, and not loading in from an
			existing JSON/INI/XML file. *)
val empty : peditor

(** Attaches a [peditor] to a [ptree] by pointing it to the root node of the
    [ptree]. *)
val peditor_of_ptree : ptree -> peditor

(** Equivalent to [cd ..] in the Unix terminal; moves from the current directory
		up to the parent directory that contains this current directory. If the
		[peditor] is already at the root, i.e. there is no parent to move to, a
		[Ptree_exception] is thrown. *)
val parent : peditor -> peditor

(** Same as the above function, except returns [None] if the [peditor] was
		already at the root instead of throwing an exception. *)
val parent_opt : peditor -> peditor option

(** Moves the [peditor] from wherever it is in the filesystem tree back to the
		root directory. Equivalent to calling [parent_opt] until [None] is
		returned. *)
val root : peditor -> peditor

(** [Ptree.cd \["one dir" ; "subdir"\] pe] moves the [peditor] [pe] into the
		subfolder of the current folder named "one dir", and then into the
		sub-subfolder "subdir" contained within "one dir". If any folder along that
		path does not exist, a [Ptree_exception] is thrown. *)
val cd : string list -> peditor -> peditor

(** Same as the above function, except returns [None] if the given path did not
		exist within the tree instead of throwing an exception. *)
val cd_opt : string list -> peditor -> peditor option

(** [Ptree.put x pe] puts [x] into the [ptree] that [pe] is attached to, at
		[pe]'s current position. If any value is already there, it will be
		overwritten. Equivalent to [Ptree.put_path x \[\] pe]. If this operation
		is attempted with a non-[Tree] [ptree] variant on the root node, a
		[Ptree_exception] is thrown. *)
val put : ptree -> peditor -> peditor

(** [Ptree.put_path x \["one dir" ; "subdir"\] pe] puts [x] into the [ptree]
		that [pe] is attached to, relative to [pe]'s current position in the
		tree. If any of the subdirectories do not exist, they will be created.
		If the path already points to an existing value, it will be overwritten. *)
val put_path : ptree -> string list -> peditor -> peditor

(** Get the [ptree] value currently pointed to by the [peditor] at its current
		position. Equivalent to [Ptree.cat_path \[\] pe]. *)
val cat : peditor -> ptree

(** [Ptree.cat_path \["one dir" ; "subdir"\] pe] gets the [ptree] value at the
		given path relative to [pe]'s current position. If any subdirectory along
		the path does not exist, a [Ptree_exception] is thrown. *)
val cat_path : string list -> peditor -> ptree

(** Same as the above function, except returns [None] if the given path did not
		exist within the tree instead of throwing an exception. *)
val cat_path_opt : string list -> peditor -> ptree option

(** Remove the [ptree] node currently pointed to by the [peditor]. The [peditor]
		is placed back at the parent node in the tree, and the removed [ptree]
		element is returned alongside the new [peditor]. If this operation is
		attempted on the root node, a [Ptree_exception] is thrown. *)
val rm : peditor -> peditor * ptree

(** Same as the above function, except returns [None] if the root was attempted
		to be deleted instead of throwing an exception. *)
val rm_opt : peditor -> (peditor * ptree) option

(** [Ptree.rm_path \["one dir" ; "subdir"\] pe] deletes the [ptree] node located
		at the specified path, and returns it in a tuple along with the new
		[peditor]. If the path points to the root, or if the path does not exist
		within the tree, a [Ptree_exception] is thrown. *)
val rm_path : string list -> peditor -> peditor * ptree

(** Same as the above function, except returns [None] if the given path did not
		exist within the tree instead of throwing an exception. *)
val rm_path_opt : string list -> peditor -> (peditor * ptree) option

(** [Ptree.mv \["from" ; "here"\] \["to" ; "there"\] pe] moves (i.e. removes
		and then reinserts) the [ptree] value at the first path (relative to [pe]'s
		current position within the tree) to the second path, relative to [pe]'s
		current position in the tree. If the first path does not exist within the
		tree, a [Ptree_exception] is thrown. If the second path does not exist
		within the tree, subdirectories will be created until it exists. *)
val mv : string list -> string list -> peditor -> peditor

(** [Ptree.cp \["from" ; "here"\] \["to" ; "there"\] pe] copies (i.e. [cat]s out
		and then inserts) the [ptree] value at the first path (relative to [pe]'s
		current position within the tree) to the second path, relative to [pe]'s
		current position in the tree. If the first path does not exist within the
		tree, a [Ptree_exception] is thrown. If the second path does not exist
		within the tree, subdirectories will be created until it exists. *)
val cp : string list -> string list -> peditor -> peditor

(** [Ptree.ls \["one dir" ; "subdir"\] pe] gets the names of all subdirectories
		at the directory located at the given path, relative to [pe]'s current
		position within the tree. If the specified path points to a leaf node
		(i.e. not a directory), or if the path does not exist within the tree, a
		[Ptree_exception] is thrown. *)
val ls : string list -> peditor -> string list

(** Same as the above function, except returns [None] instead of throwing an
		exception. *)
val ls_opt : string list -> peditor -> string list option

(** Get the path pointing to the [peditor]'s current position within the tree,
		relative to the root. *)
val pwd : peditor -> string list

(** Get the depth of the sub-[ptree] under the [peditor]'s current position. To
		get the depth of the whole tree, use [Ptree.root] first followed by
		[Ptree.depth]. This function is especially useful for ensuring the [ptree]
		can be properly converted to a certain file format, especially one that
		does not support lots of nesting like the INI format. *)
val depth : peditor -> int

(** Parse the string containing INI into a [peditor] object (which will be
		pointing at the root). If there is a parse error, a [Ptree_exception] will
		be thrown with the parse error message. *)
val peditor_of_ini : string -> peditor

(** Same as the above function, except returns [None] if parsing was
		unsuccessful instead of throwing an exception. *)
val peditor_of_ini_opt : string -> peditor option

(** Parse the string containing JSON into a [peditor] object (which will be
		pointing at the root). If there is a parse error, a [Ptree_exception] will
		be thrown with the parse error message. *)
val peditor_of_json : string -> peditor

(** Same as the above function, except returns [None] if parsing was
		unsuccessful instead of throwing an exception. *)
val peditor_of_json_opt : string -> peditor option

(** Parse the string containing XML into a [peditor] object (which will be
		pointing at the root). If there is a parse error, a [Ptree_exception] will
		be thrown with the parse error message. *)
val peditor_of_xml : string -> peditor

(** Same as the above function, except returns [None] if parsing was
		unsuccessful instead of throwing an exception. *)
val peditor_of_xml_opt : string -> peditor option

(** Validates an INI key (ensuring it does not contain any illegal characters),
		acting like the identity function and returning the string itself if it is
		a valid key. If not, a [Ptree_exception] is thrown. *)
val validate_ini_key : string -> string

(** Validates an XML key (ensuring it does not contain any illegal characters),
		acting like the identity function and returning the string itself if it is
		a valid key. If not, a [Ptree_exception] is thrown. *)
val validate_xml_key : string -> string

(** Convert the entire [ptree] pointed to by the [peditor] (not just the subtree
		under the [peditor]) into an INI string. If the depth of the [ptree] is
		greater than two (i.e. if it contains sub-subdirectories) a
		[Ptree_exception] is thrown, as the INI format does not support such
		nesting. *)
val ini_of_peditor : peditor -> string

(** Convert the entire [ptree] pointed to by the [peditor] (not just the subtree
		under the [peditor]) into a JSON string. The string argument specifies the
		string used as the tab character. Please ensure that that argument consists
		solely of whitespace; if not, invalid JSON can be returned. *)
val json_of_peditor_tab : peditor -> string -> string

(** Convert the entire [ptree] pointed to by the [peditor] (not just the subtree
		under the [peditor]) into a JSON string. A default indent of two spaces is
		used to pretty-print the JSON. *)
val json_of_peditor : peditor -> string

(** Convert the entire [ptree] pointed to by the [peditor] (not just the subtree
		under the [peditor]) into an XML string. The string argument specifies the
		string used as the tab character. Please ensure that that argument consists
		solely of whitespace; if not, invalid XML can be returned. *)
val xml_of_peditor_tab : peditor -> string -> string -> string

(** Convert the entire [ptree] pointed to by the [peditor] (not just the subtree
		under the [peditor]) into an XML string. A default indent of two spaces is
		used to pretty-print the XML. *)
val xml_of_peditor : peditor -> string -> string

(** An enumeration of the file types currently supported by the library. *)
type supported_types = JSON | XML | INI

(** Parse the file containing the specified file type at the given path into a
		[peditor] object (which will be pointing at the root). If there is a parse
		error, a [Ptree_exception] will be thrown with the parse error message. *)
val peditor_of_file_type : string -> supported_types -> peditor

(** Parse the file into a [peditor] object (which will be pointing at the root).
		The file format will be detected based on the file extension (i.e.
		expecting one of ".ini", ".json", and ".xml"). If the desired file does
		not have that extension, or if the format is already known, prefer to use
		[peditor_of_file_type]. If there is a parse error, a [Ptree_exception]
		will be thrown with the parse error message. *)
val peditor_of_file : string -> peditor

(** Write out the entire [ptree] pointed to by the [peditor] in the specified
		format to the file at the given path. *)
val file_of_peditor_type : string -> supported_types -> peditor -> unit

(** Write out the entire [ptree] pointed to by the [peditor] to the file at the
		given path. The format to convert the [ptree] to will be detected based on
		the file extension (i.e. expecting one of ".ini", ".json", and ".xml"). *)
val file_of_peditor : string -> peditor -> unit
