open OUnit2
open Ptree

(* The basic testing strategy is going to be
   - Build up a variety of small trees using peditor actions
   - Check those trees are correct using peditor actions, ptree equality
   - Make sure peditor throws exceptions on illegal movements
   - Pretty-print out those test trees in each of the different formats
   - Parse some test files into ptree and then check with peditor actions *)
let one_rel = empty 
  |> put_path (String "Hello testing = needs quotes") ["field1"]

let all_rels = one_rel
  |> put_path (Int 12L) ["another_field"]
  |> put_path (Float 3.14159265) ["thirdfield"]
  |> put_path (Boolean false) ["4th-field"]
  |> cd ["4th-field"]
  |> put (Boolean true)
  |> parent
  |> put_path Null ["field5"]
  |> put_path (Array [| (String "t\nt") ; (String "Com,ma") ; Null ; (Float 2.71828) |]) ["fsix"]

let depth_one = all_rels
  |> cd ["another_field"]
  |> put_path (String "subdata") ["subf"]
  |> put_path (Int 100L) ["subfield2"]
  |> root
  |> cp ["another_field"] ["newSubtree"]
  |> rm_path ["another_field" ; "subf"]
  |> fst
  |> mv ["newSubtree" ; "subfield2"] ["newSubtree" ; "moved"]

let depth_two = depth_one
  |> put_path (Boolean false) ["another_field" ; "asdf"; "randomdata"]
  |> cd ["newSubtree" ; "subf"]
  |> rm
  |> fst
  |> root

let tests = "Ptree tests" >::: [
  ("depth_one" >:: fun _ -> print_string (xml_of_peditor depth_two "testtag"));

  (************ `depth` tests ***************)
  ("depth = 0" >:: fun _ -> assert_equal 0 (depth empty));
  ("depth = 1" >:: fun _ -> assert_equal 1 (depth all_rels));
  ("depth = 2" >:: fun _ -> assert_equal 2 (depth depth_one));
  ("depth = 3" >:: fun _ -> assert_equal 3 (depth depth_two));

  (************* `pwd` tests *****************)
  ("pwd empty" >:: fun _ -> assert_equal [] (pwd empty));
  ("pwd root" >:: fun _ -> assert_equal [] (pwd depth_two));
  ("pwd one elt" >:: fun _ -> assert_equal ["4th-field"] (pwd (cd ["4th-field"] depth_two)));
  ("another pwd test" >:: fun _ -> assert_equal ["another_field" ; "asdf"]
    (depth_two |> cd ["another_field" ; "asdf" ; "randomdata"] |> parent |> pwd));
  
  (************* `ls` tests *******************)
  ("ls empty" >:: fun _ -> assert_equal [] (ls [] empty));
  ("ls leaf node" >:: fun _ -> assert_raises
    (Ptree_exception "cannot cast, this ptree is not a Tree node.")
    (fun () -> ls ["field1"] one_rel));
  ("ls regular test" >:: fun _ -> assert_equal
    ["asdf" ; "subfield2"] (ls ["another_field"] depth_two));
  ("ls-opt fail" >:: fun _ -> assert_equal None (ls_opt ["nonexist"] one_rel));
  ("ls-opt pass" >:: fun _ -> assert_equal (Some ["field1"]) (ls_opt [] one_rel));

  (************** `cat` tests ******************)
  ("cat" >:: fun _ -> assert_equal (Int 12L) (all_rels |> cd ["another_field"] |> cat));
  ("cat_path basic" >:: fun _ -> assert_equal (Int 12L) (cat_path ["another_field"] all_rels));
  ("cat_path fails" >:: fun _ -> assert_raises
    (Ptree_exception "Key not found; cannot cd.")
    (fun () -> cat_path ["asdfghjl"] all_rels));
  ("cat_path_opt pass" >:: fun _ -> assert_equal (Some (Int 12L)) (cat_path_opt ["another_field"] all_rels));
  ("cat_path_opt fail" >:: fun _ -> assert_equal None (cat_path_opt ["asdfhjkl"] all_rels));

  (************* `cd_opt` tests ****************)
  (* Don't really see the need to separately test cd since it's been already used
     pretty extensively by now *)
  ("cd_opt fail" >:: fun _ -> assert_equal None (cd_opt ["qwertyuij"] all_rels));

  (************** `parent` tests ***************)
  ("parent fails" >:: fun _ -> assert_raises
    (Ptree_exception "Already at the root; no parent to go to.")
    (fun () -> parent depth_two));
  ("parent_opt fails" >:: fun _ -> assert_equal None (parent_opt depth_two));

  (************* ini pprinting tests *************)
  ("ini empty" >:: fun _ -> assert_equal "\n" (ini_of_peditor empty));
  ("ini one_rel" >:: fun _ -> assert_equal
    "field1 = \"Hello testing = needs quotes\"\n"
    (ini_of_peditor one_rel));
  ("ini all_rels" >:: fun _ -> assert_equal
"4th-field = true
another_field = 12
field1 = \"Hello testing = needs quotes\"
field5 = 
fsix = [\"t\\nt\", \"Com,ma\", , 2.718280]
thirdfield = 3.141593
"
    (ini_of_peditor all_rels));
  ("ini depth_one" >:: fun _ -> assert_equal
"4th-field = true
field1 = \"Hello testing = needs quotes\"
field5 = 
fsix = [\"t\\nt\", \"Com,ma\", , 2.718280]
thirdfield = 3.141593

[another_field]
subfield2 = 100

[newSubtree]
moved = 100
subf = subdata
"
    (ini_of_peditor depth_one));

  (* INI can't print depth_two *)
  ("ini depth_two" >:: fun _ -> assert_raises
    (Ptree_exception "Cannot convert a tree of depth > 2 to INI format")
    (fun () -> ini_of_peditor depth_two));

  (*************** JSON pprinting tests **************)
  ("json empty" >:: fun _ -> assert_equal "{}\n" (json_of_peditor empty));
  ("json one_rel" >:: fun _ -> assert_equal
"{
  \"field1\": \"Hello testing = needs quotes\"
}
"
    (json_of_peditor one_rel));
  ("json all_rels" >:: fun _ -> assert_equal
"{
  \"4th-field\": true,
  \"another_field\": 12,
  \"field1\": \"Hello testing = needs quotes\",
  \"field5\": null,
  \"fsix\": [
    \"t\\nt\",
    \"Com,ma\",
    null,
    2.718280
  ],
  \"thirdfield\": 3.141593
}
"
    (json_of_peditor all_rels));
  
  ("json depth_two" >:: fun _ -> assert_equal
"{
  \"4th-field\": true,
  \"another_field\": {
    \"asdf\": {
      \"randomdata\": false
    },
    \"subfield2\": 100
  },
  \"field1\": \"Hello testing = needs quotes\",
  \"field5\": null,
  \"fsix\": [
    \"t\\nt\",
    \"Com,ma\",
    null,
    2.718280
  ],
  \"newSubtree\": {
    \"moved\": 100
  },
  \"thirdfield\": 3.141593
}
"
    (json_of_peditor depth_two));

    (************** XML pprinting tests *****************)
    ("xml empty" >:: fun _ -> assert_equal "<test>\n\n</test>\n" (xml_of_peditor empty "test"));
    ("xml depth_two" >:: fun _ -> assert_equal
"<testtag>
  <4th-field>
true
  </4th-field>
  <another_field>
    <asdf>
      <randomdata>
        false
      </randomdata>
    </asdf>
    <subfield2>
      100
    </subfield2>
  </another_field>
  <field1>
    Hello testing = needs quotes
  </field1>
  <field5>
    null
  </field5>
  <fsix>
    <fsix.item>
      t
t
    </fsix.item>
    <fsix.item>
      Com,ma
    </fsix.item>
    <fsix.item>
      null
    </fsix.item>
    <fsix.item>
      2.718280
    </fsix.item>
  </fsix>
  <newSubtree>
    <moved>
      100
    </moved>
  </newSubtree>
  <thirdfield>
    3.141593
  </thirdfield>
</testtag>
"
    (xml_of_peditor depth_two "testtag"));
  ]


let _ = run_test_tt_main tests
