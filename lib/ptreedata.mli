(** For documentation, see ptree.mli. *)

module StrDict :
  sig
    type key = string
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
exception Ptree_exception of string
type ptree =
    Int of int64
  | Float of float
  | String of string
  | Boolean of bool
  | Null
  | Array of ptree array
  | Tree of ptree StrDict.t
type peditor = { under : ptree; above : (string * peditor) option; }
val get_int : ptree -> int64
val get_float : ptree -> float
val get_string : ptree -> string
val get_boolean : ptree -> bool
val get_array : ptree -> ptree array
val get_tree : ptree -> ptree StrDict.t
val empty : peditor
val peditor_of_ptree : ptree -> peditor
val parent : peditor -> peditor
val parent_opt : peditor -> peditor option
val root : peditor -> peditor
val cd : string list -> peditor -> peditor
val cd_opt : string list -> peditor -> peditor option
val put : ptree -> peditor -> peditor
val put_path : ptree -> string list -> peditor -> peditor
val cat : peditor -> ptree
val cat_path : string list -> peditor -> ptree
val cat_path_opt : string list -> peditor -> ptree option
val rm : peditor -> peditor * ptree
val rm_opt : peditor -> (peditor * ptree) option
val rm_path : string list -> peditor -> peditor * ptree
val rm_path_opt : string list -> peditor -> (peditor * ptree) option
val mv : string list -> string list -> peditor -> peditor
val cp : string list -> string list -> peditor -> peditor
val ls : string list -> peditor -> string list
val ls_opt : string list -> peditor -> string list option
val pwd : peditor -> string list
val depth : peditor -> int
