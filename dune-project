(lang dune 3.15)

(name MiniLang)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Donaldo Buzi")

(maintainers "Donaldo Buzi")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name MiniLang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ocamllex menhir)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
