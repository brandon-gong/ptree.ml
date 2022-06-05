(lang dune 3.0)

(name ptreeml)

(generate_opam_files true)

(source
 (github brandon-gong/ptreeml))

(authors "Brandon Gong")

(license LICENSE)

(package
 (name ptreeml)
 (synopsis "boost::property_tree for OCaml - a multipurpose data
            serialization/deserialization library")
 (description "ptreeml provides a unified abstraction to parse, convert between,
               or generate file formats including XML, JSON, and YAML. It is
               inspired by C++'s Boost.PropertyTree library, though it does not
               implement INI or INFO parsers (as of now) as these file formats
               are either deprecated or not often used.")
 (depends ocaml dune)
 (tags (topics json yaml xml "property tree")))
