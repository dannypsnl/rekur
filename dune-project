(lang dune 3.14)

(name rekur)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name rekur)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (eio_main
   (>= 0.12))
  (asai
   (>= 0.2.0))
  (yuujinchou
   (>= 5.1.0))
  (algaeff
   (>= 2.0.0))
  (menhir
   (>= 20211230)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
