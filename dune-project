(lang dune 2.9)

(using menhir 2.1)

(name owi)

(license ISC)

(authors "Léo Andrès <contact@ndrs.fr>" "Pierre Chambart <pierre.chambart@ocamlpro.com>")

(maintainers "Léo Andrès <contact@ndrs.fr>")

(source
 (github ocamlpro/owi))

(generate_opam_files true)

(package
 (name owi)
 (synopsis "OCaml library/executable to TODO")
 (description "owi is an OCaml library/executable to TODO.")
 (tags
  (owi TODO TODO TODO TODO))
 (depends
  (ocaml
   (>= 4.12))
  (integers
   (>= 0.5.1))
  sedlex
  (menhir
   (and
    :build
    (>= 20180528)))
  ocaml_intrinsics
  uutf
  (bisect_ppx
   (and
    :with-test
    (>= "2.5")
    :dev))
  (ocb
   (and
    :with-test
    (>= "0.1")
    :dev))
  (odoc :with-doc)
  (bos :with-test)))
