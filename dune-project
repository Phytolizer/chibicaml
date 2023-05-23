(lang dune 3.7)

(name chibic)

(generate_opam_files true)

(source
 (github Phytolizer/chibicaml))

(authors "Kyle Coffey")

(maintainers "Kyle Coffey")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name chibic)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune angstrom ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
