(lang dune 3.0)

(using menhir 2.1)
(using dune_site 0.1)
(using mdx 0.2)
(using directory-targets 0.1)

(cram enable)

(generate_opam_files true)

(name owi)

(license AGPL-3.0-or-later)

(authors
 "Léo Andrès <leo@ocamlpro.com>"
 "Pierre Chambart <pierre.chambart@ocamlpro.com>"
 "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>"
 "Eric Patrizio <epatrizio@mpns.fr>"
 "Arthur Carcano <arthur.carcano@ocamlpro.com>")

(maintainers "Léo Andrès <leo@ocamlpro.com>")

(source
 (github ocamlpro/owi))

(package
 (name owi)
 (synopsis
  "OCaml toolchain to work with WebAssembly, including an interpreter")
 (description
  "Owi is an OCaml toolchain to work with WebAssembly. It provides an interpreter as an executable and a library.")
 (tags
  (owi ocaml webassembly wasm interpreter compiler))
 (depends
   ;; build
  (ocaml
   (>= 5.1))
  (menhir
   (and
    :build
    (>= 20220210)))
  (dune-site :build)
  ;; runtime
  bos
  cmdliner
  conf-clang
  digestif
  (integers
   (>= 0.5.1))
  ocaml_intrinsics
  (prelude (>= 0.3))
  sedlex
  (smtml (>= 0.3.1))
  uutf
  xmlm
  (processor (>= 0.2))
  ;; doc
  (odoc :with-doc)
  ;; test
  (mdx
   (and
    :with-test
    (>= 2.1)))
  (frama-c (and (>= 29.0) :with-test))
  ;; dev
  (bisect_ppx
   (and
    (>= 2.5)
    :with-dev-setup))
  (ocb
   (and
    (>= 0.1)
    :with-dev-setup))
  (crunch :with-dev-setup)
  (extunix :with-dev-setup)
  (crowbar :with-dev-setup)
  (gnuplot :with-dev-setup)
  (graphics :with-dev-setup)
  (tiny_httpd :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (rusage :with-dev-setup)
  (tyxml :with-dev-setup)
  (yaml :with-dev-setup)
  (lwt :with-dev-setup)
  (cohttp :with-dev-setup)
  (cohttp-lwt-unix :with-dev-setup)
  )
 (sites (share binc) (share libc)))
