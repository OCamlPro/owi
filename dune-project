(lang dune 3.7)

(map_workspace_root false)

(using menhir 2.1)

(using dune_site 0.1)

(using mdx 0.2)

(using directory-targets 0.1)

(cram enable)

(generate_opam_files true)

(name owi)

(license AGPL-3.0-or-later)

(authors
 "redianthus <leo@ocaml.pro>"
 "Pierre Chambart <pierre.chambart@ocamlpro.com>"
 "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>"
 "Eric Patrizio <epatrizio@mpns.fr>"
 "Arthur Carcano <arthur.carcano@ocamlpro.com>")

(maintainers "redianthus <leo@ocaml.pro>")

(source
 (github ocamlpro/owi))

(package
 (name owi)
 (synopsis
  "Symbolic execution engine and WebAssembly toolkit for research and engineering")
 (description
  "Owi is an open-source framework for symbolic execution and WebAssembly analysis.\nIt supports the analysis of programs written in WebAssembly, C, C++, Rust, and Zig.\nOwi provides symbolic execution for bug finding, solver-aided programming, test case generation, and program verification.\n\nThe framework includes tools for formatting, optimizing, interpreting, validating, and translating WebAssembly modules.\nIt also provides a generator for random valid WebAssembly modules.\n\nAn OCaml library is available to embed WebAssembly modules and import OCaml functions in a type-safe manner.")
 (tags
  (owi ocaml webassembly wasm interpreter compiler))
 (depends
  ;; build
  (ocaml
   (>= 5.2))
  (menhir
   (and
    :build
    (>= 20220210)))
  (dune-build-info :build)
  (dune-site :build)
  ;; system packages
  conf-clang
  ;; conf-lld does not have `wasm-ld` on macOS and compilation fails
  ;; conf-llvm ;; this package is broken on opam-repository: on Debian it'll install the llvm-X but not llvm package...
  ;; conf-rust-llvm ;; not available in ubuntu
  ;; conf-rust-wasm ;; libstd-rust-dev-wasm32 is not available in ubuntu
  ;;conf-zig
  ;; runtime
  (bos
   (>= 0.2.1))
  (cmdliner
   (>= 1.3.0))
  (digestif
   (>= 1.2.0))
  (logs
   (>= 0.10.0))
  (ocaml_intrinsics
   (>= "v0.16.1"))
  (prelude
   (>= 0.5))
  (processor
   (>= 0.2))
  (sedlex
   (>= 3.3))
  (smtml
   (>= 0.15.0))
  synchronizer
  (uutf
   (>= 1.0.3))
  (xmlm
   (>= 1.4.0))
  ;; doc
  (odoc
   (and
    (>= 3.0.0)
    :with-doc))
  ;; test
  (mdx
   (and
    :with-test
    (>= 2.1)))
  (frama-c
   (and
    (>= 29.0)
    :with-test))
  (z3 :with-test)
  ;; dev
  (bisect_ppx
   (and
    (>= 2.5)
    :with-dev-setup))
  (crowbar :with-dev-setup)
  (landmarks-ppx :with-dev-setup)
  (ocamlformat
   (and
    (>= 0.28)
    :with-dev-setup))
  (ocb
   (and
    (>= 0.1)
    :with-dev-setup)))
 (sites
  (share c_files)
  (share rust_files)
  (share zig_files)))
