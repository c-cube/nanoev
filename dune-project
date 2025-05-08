(lang dune 2.7)

(name nanoev)

(generate_opam_files true)

(source
 (github c-cube/nanoev))

(authors "Simon Cruanes")

(maintainers "Simon Cruanes")

(version 0.1)

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name nanoev)
 (synopsis "Tiny event loop abstraction")
 (depends
  (ocaml
   (>= 4.12))
  dune
  base-unix)
 (depopts
  (trace
   (>= 0.7)))
 (tags
  (unix select async)))

(package
 (name nanoev-picos)
 (synopsis "Use nanoev from picos")
 (depends
  (ocaml
   (>= 4.14))
  dune
  base-unix
  (nanoev
   (= :version))
  (iostream
   (>= 0.3))
  (picos
   (and
    (>= 0.5)
    (< 0.7)))
  (picos_std
   (and
    (>= 0.5)
    (< 0.7))))
 (tags
  (unix select async)))

(package
 (name nanoev-posix)
 (synopsis "Use mtime+iomux (posix compliant) as a backend for nanoev")
 (depends
  (ocaml
   (>= 4.12))
  dune
  base-unix
  (iomux (>= 0.3))
  (nanoev
   (= :version))
  (nanoev-picos
   (= :version))
  (mtime
   (>= 2.0))
  (moonpool :with-test)
  (trace :with-test)
  (trace-tef :with-test))
 (tags
  (unix select async iomux nanoev)))

(package
 (name nanoev_tiny_httpd)
 (synopsis "Use nanoev as a basis for tiny_httpd")
 (depends
  ocaml
  dune
  (nanoev
   (= :version))
  (nanoev-picos
   (= :version))
  picos
  picos_std
  (tiny_httpd
   (>= 0.19)))
 (tags
  (nanoev http)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
