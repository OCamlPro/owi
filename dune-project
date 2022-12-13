(lang dune 3.0)

(using menhir 2.1)

(using mdx 0.2)

(name owi)

(license ISC)

(authors
 "L\195\169o Andr\195\168s <contact@ndrs.fr>"
 "Pierre Chambart <pierre.chambart@ocamlpro.com>")

(maintainers "L\195\169o Andr\195\168s <contact@ndrs.fr>")

(source
 (github ocamlpro/owi))

(generate_opam_files true)

(package
 (name owi)
 (synopsis
  "OCaml toolchain to work with WebAssembly, including and interpreter")
 (description
  "owi is an OCaml toolchain to work with WebAssembly. It provides an interpreter as an executable and a library.")
 (tags
  (owi ocaml webassembly wasm interpreter compiler))
 (depends
  (ocaml
   (>= 4.13))
  (integers
   (>= 0.5.1))
  cmdliner
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
  (bos :with-test)
  (mdx
   (and
    :with-test
    (>= "2.1")))))
