open Ptreedata

val validate_ini_key : string -> string
val ini_of_peditor : peditor -> string
val json_of_peditor_tab : string -> peditor -> string
val json_of_peditor : peditor -> string
val validate_xml_key : string -> string
val xml_of_peditor_tab : string -> string -> peditor -> string
val xml_of_peditor : string -> peditor -> string
