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
  (dune-build-info :build)
  (dune-site :build)
  ;; system packages
  conf-clang
  ;;conf-lld
  ;;conf-llvm
  ;;conf-rust-llvm
  ;;conf-rust-wasm
  ;;conf-zig
  ;; runtime
  (bos
    (>= 0.2.1))
  (cmdliner
    (>= 1.3.0))
  (digestif
    (>= 1.2.0))
  (integers
   (>= 0.5.1))
  (ocaml_intrinsics
    (>= "v0.17.0"))
  (prelude
    (>= 0.5))
  (processor
    (>= 0.2))
  (sedlex
    (>= 3.3))
  (smtml
    (>= 0.6.0))
  (uutf
    (>= 1.0.3))
  (xmlm (>= 1.4.0))
  ;; doc
  (odoc (and (>= "3.0.0~beta1") :with-doc))
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
  (lwt_ssl :with-dev-setup)
  )
 (sites (share c_files) (share rust_files) (share zig_files)))
