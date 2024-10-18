(lang dune 2.7)

(name nanoev)
(generate_opam_files true)

(source
 (github c-cube/nanoev))

(authors "Simon Cruanes")

(maintainers "Simon Cruanes")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name nanoev)
 (synopsis "Tiny event loop around `select`")
 (depends ocaml dune base-unix)
 (tags (unix select async)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
