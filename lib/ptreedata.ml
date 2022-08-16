(*
  This module contains declarations for the ptree and peditor data types, and
  implementations for editing / movement with the peditor.

  Comments here are mainly implementation-specific; for more user-oriented docs,
  see ptree.mli.

  The design of the peditor data type is inspired by the Zipper data structure,
  except it mainly focuses on up / down navigation and not lateral motion
  between siblings.
*)


(***************** Data types *****************)

(* We'll derive a StrDict module here using the Map.Make functor. A StrDict is
   simply a mapping from a string key to any other value (in our case, typically
   it'll be from strings to ptree values). I chose Map over Hashtbl because I
   wanted it to be immutable and persistent. *)
module StrDict = Map.Make(String)

(* A custom exception type for the ptree library. This is to allow ptree.ml to
   be used alongside other libraries without errors getting mixed up. It'll be
   thrown whenever an invalid action is attempted with peditor, or if some parse
   error occurs, or if some issue arises when we try to pretty-print ptrees. *)
exception Ptree_exception of string

(* The ptree type. Decided to go with int64 since it'll be more broadly
   applicable, but it might make memory usage less efficient in some cases. *)
type ptree =
  | Int of int64
  | Float of float
  | String of string
  | Boolean of bool
  | Null
  | Array of ptree array
  | Tree of ptree StrDict.t

(* The peditor type. As in Zippers, we store the subtree that is currently
   pointed to by the Zipper, and we store the parent Zipper that this zipper is
   a child of. Unlike some zipper implementations, we don't store any
   neighboring right or left nodes, since we don't care about lateral motion.
   
   In addition to the parent peditor, we also store the key that maps to the
   subtree that the current peditor points to in the parent StrDict. This allows
   us to make the necessary update operations when we pop back up to the parent
   peditor. *)
type peditor =
  {
    under: ptree;
    above: (string * peditor) option;
  }


(********************** Utility functions ***********************)

(* Apply f to x, n times. *)
let rec apply_n n f x = if n <= 0 then x else apply_n (n-1) f (f x)

(* Two helper functions to make functions with one argument and two arguments
   respectively return options instead of throwing exceptions *)
let make_opt1 f x = try Some (f x) with Ptree_exception _ -> None
let make_opt2 f x y= try Some (f x y) with Ptree_exception _ -> None


(********************* ptree access functions ********************)

(* In many cases, we know for sure what data type a certain place in the tree
   will be. In these cases, it's cumbersome to write a whole match statement 
   just to pick it out. *)

let get_int = function
  | Int i -> i
  | _ -> raise @@ Ptree_exception "cannot cast, this ptree is not an Int node."

let get_float = function
  | Float f -> f
  | _ -> raise @@ Ptree_exception "cannot cast, this ptree is not a Float node."

let get_string = function
  | String s -> s
  | _ -> raise @@ Ptree_exception "cannot cast, this ptree is not a String node."

let get_boolean = function
  | Boolean b -> b
  | _ -> raise @@ Ptree_exception "cannot cast, this ptree is not a Bool node."

let get_array = function
  | Array a -> a
  | _ -> raise @@ Ptree_exception "cannot cast, this ptree is not an Array node."

let get_tree = function
  | Tree t -> t
  | _ -> raise @@ Ptree_exception "cannot cast, this ptree is not a Tree node."


(********************** peditor specific functions *******************)

(* An empty peditor points to a root tree with no bindings, and no parent
   directory. *)
let empty = { under = Tree(StrDict.empty); above = None; }

(* Support creating a new peditor from an existing tree or subtree *)
let peditor_of_ptree pt = match pt with
  | Tree _ -> { under = pt; above = None; }
  | _ -> raise @@ Ptree_exception "Cannot create peditor from non-Tree node."


(* Going to parent directory we need to do two things: check if there even is a
   parent (if not, exception), and if there is one, we need to change the
   reference in the parent's StrDict to point to this (potentially updated)
   subtree. *)
let parent pe = match pe.above with
  | None ->
    (* This peditor has nothing above it, raise exception. *)
    raise @@ Ptree_exception "Already at the root; no parent to go to."

  | Some (k, ppe) ->
    (* It does have a parent peditor, ppe, and its key in that parent is k *)
    let updated = get_tree ppe.under |> StrDict.add k pe.under in
    { ppe with under = Tree updated }

let parent_opt = make_opt1 parent


(* This is a common enough operation, so it's included with the library. Simply
   calls `parent` on the peditor until there is no parent remaining. *)
let rec root pe = match parent_opt pe with
  | None -> pe
  | Some ppe -> root ppe


(* cd_single is included as an inner function and not exposed since it's
   functionality would be redundant with cd ["single_folder"] pe. That said,
   it contains the bulk of the functionality. *)
let cd path pe =
  let cd_single p dir_name =
    match p.under with
    (* First check to make sure p is on a Tree node. I could use tree_of_ptree
       here, but I'm just doing it manually, as the error message will be a bit
       more useful to the reader. *)
    | Tree t -> (
      (* We are indeed on a Tree node, so now try to locate the desired dir in
         the node.*)  
      match StrDict.find_opt dir_name t with
      | Some subdir -> {under = subdir; above = Some (dir_name, p)}
      | None -> raise @@ Ptree_exception "Key not found; cannot cd.")
    | _ ->
      (* We are not on a Tree node, so we can't cd into anything. *)
      raise @@ Ptree_exception "peditor is not on a Tree; cannot cd." in

  List.fold_left cd_single pe path

let cd_opt = make_opt2 cd


(* Replaces the node currently pointed to by the peditor with the given ptree
   value. Here, we do need to do some work to make sure we aren't replacing the
   root node with some non-Tree value. *)
let put pt pe =
  let modified = {pe with under = pt} in
  (* Check to see if this node is the root node (i.e. nothing above it) *)
  match pe.above with
  | None -> (
    (* Since we are on the root, we have to make sure pt is a tree. *)
    match pt with
    | Tree _ -> modified
    | _ -> raise @@ Ptree_exception "Cannot set root node to non-dict value.")
  | Some _ -> modified


(* Put the given ptree value at the given path, relative to the peditor. The
   reason why I don't use cd and then put here is that cd will fail if it tries
    to enter a directory that doesn't exist, while I want put_path to create
    new directories all the way up until the point that it needs to insert. *)
let rec put_path pt path pe = match path with
  | [] ->
    (* `path` is empty, so we just put the value here and return. *)
    put pt pe
  | f::r -> (match cd_opt [f] pe with
    | Some p ->
      (* We were able to cd into this subdirectory; now just recur on put_path
         for the remaining subdirs and, when done, call `parent` to cd back out
         to the original directory. *)
      parent @@ put_path pt r p
    | None ->
      let mt_tree = Tree StrDict.empty in
      let add_mt = StrDict.add f mt_tree in
      (* Ok, so the subdirectory doesn't exist, which is why cd failed. Now,
         we check if our peditor is currently on a Tree node. *)
      match pe.under with
      | Tree t ->
        (* If so, great! We can simply add the new key into the dictionary
           so that we can cd into it next time we try. *)
        put_path pt path {pe with under = Tree (add_mt t)}
      | _ ->
        (* If not, we'll have to replace whatever's there with an empty dict.
           We then add our desired key as a subdirectory to this new dict. *)
        put_path pt path {pe with under = Tree (add_mt StrDict.empty)})


let cat pe = pe.under


(* Unlike put_path, here we can use cd, because if it fails we have nothing to
   cat. *)
let cat_path path pe = cd path pe |> cat

let cat_path_opt = make_opt2 cat_path

(* Remove the node that the peditor is currently pointing to. I've decided to
   make it return a pair with the new peditor and the element that was removed,
   since I felt it might be needed sometimes. This will make chaining calls
   slightly harder, but luckily it's easy to pull out the peditor by piping to
   |> fst, or the removed ptree by piping to |> snd. *)
let rm pe =
  (* This is pretty straightforward, we just need to make sure we aren't
     deleting the root. *)
  match pe.above with
  | Some (k, p) -> let t = get_tree p.under in
    {p with under = Tree (StrDict.remove k t)}, StrDict.find k t
  | None -> raise @@ Ptree_exception "You cannot rm the root directory."

let rm_opt = make_opt1 rm

(* Here we can also use cd, as removing a directory that doesn't exist just
   makes no sense. Since we're returning the peditor, unlike cat we need to 
   leave the peditor in the same spot we found it, so we call parent to pop
   all the way back out to where we were. *)
let rm_path path pe =
  let pop_out = apply_n (List.length path - 1) parent in
  cd path pe
  |> rm
  |> fun (p, d) -> (pop_out p, d)

let rm_path_opt = make_opt2 rm_path


(* Since we return what we just deleted, mv comes for free, as we just remove
   from one path and put it at the destination. *)
let mv source dest pe = rm_path source pe |> fun (p, d) -> put_path d dest p

let cp source dest pe = cat_path source pe |> fun d -> put_path d dest pe

let ls path pe = cd path pe
  |> (fun p -> p.under)
  |> get_tree
  |> StrDict.bindings
  |> List.map fst

let ls_opt = make_opt2 ls

let pwd pe =
  let rec aux t = match t.above with
    | None -> []
    | Some (k, p) -> k :: (aux p) in
  List.rev (aux pe)

(* Not sure when else this will be useful, but I use it when preparing to pprint
   out to an INI file format, as INI does not support anything with depth more
   more than two. This is basically your classic tree depth algorithm, adapted
   to work with our peditor type. *)
let rec depth pe =
  let keep_max m x = max m (depth @@ cd [x] pe) in
  match ls_opt [] pe with
  | None | Some [] -> 0
  | Some lst -> 1 + List.fold_left keep_max 0 lst

