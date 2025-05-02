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
 (synopsis "Tiny event loop abstraction")
 (depends ocaml dune base-unix)
 (depopts
  (trace
   (>= 0.7))
  (iostream
   (>= 0.3))
  (picos
   (and
    (>= 0.5)
    (< 0.7))))
 (tags
  (unix select async)))

(package
 (name nanoev-posix)
 (synopsis "Use mtime+iomux (posix compliant) as a backend for nanoev")
 (depends ocaml dune base-unix iomux (mtime (>= 2.0)))
 (tags
  (unix select async iomux nanoev)))

(package
 (name nanoev_tiny_httpd)
 (synopsis "Use nanoev as a basis for tiny_httpd")
 (depends
  ocaml
  dune
  nanoev
  (picos
   (>= 0.6))
  picos_std
  (tiny_httpd
   (>= 0.17)))
 (tags
  (nanoev http)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
