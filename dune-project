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
 (depopts
  (picos
   (and (>= 0.5) (< 0.7))))
 (tags
  (unix select async)))

(package
 (name nanoev_tiny_httpd)
 (synopsis "Use nanoev as a basis for tiny_httpd")
 (depends
  ocaml
  dune
  nanoev
  picos
  (tiny_httpd (>= 0.17))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
