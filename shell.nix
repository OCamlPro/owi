{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  smtml = pkgs.ocamlPackages.smtml.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "formalsec";
      repo = "smtml";
      rev = "e259d5b7d108cb2d1102c188132a6c86bbf7705e";
      hash = "sha256-8OzNJIyJhfqgDw3ioINN0D0WVTSliG9TP4cUNlrm4s8=";
    };
    doCheck = false;
  });
  domainpc = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {

    pname = "domainpc";
    version = "0.1";

    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "domainpc";
      rev = "9d5596c57154d925c6d7f17fe37dc2b9da0434bc";
      hash = "sha256-ttuv/G2J+7DsmDMMRtUOT1jITUtCoj9qQUXzfJ/yRfc=";
    };

    propagatedBuildInputs = [
      pkgs.ocamlPackages.processor
    ];
  });
  synchronizer = pkgs.ocamlPackages.synchronizer.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "synchronizer";
      rev = "cb5fd995e8a42e5244acf68f238221594fd19a8d";
      hash = "sha256-0XtPHpDlyH1h8W2ZlRvJbZjCN9WP5mzk2N01WFd8eLQ=";
    };
  });
  landmarks = pkgs.ocamlPackages.landmarks.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "hra687261";
      repo = "landmarks";
      rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
      hash = "sha256-3ui4uvSAvUgzk2UMVtH9A4BhAX6nWbwx7q0YwkANNv8=";
    };
  });
  landmarks-ppx = pkgs.ocamlPackages.landmarks-ppx.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "hra687261";
      repo = "landmarks";
      rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
      hash = "sha256-3ui4uvSAvUgzk2UMVtH9A4BhAX6nWbwx7q0YwkANNv8=";
    };
    meta.broken = false;
  });
  symex = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {

    pname = "symex";
    version = "0.1";

    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "symex";
      rev = "c9241e0084818f63b5fe915dc353dc6db713a249";
      hash = "sha256-jKwFtxVcBD8Y1bfKRB8Z/MSeQLQWKvk00i8HqodkBbM=";
    };

    propagatedBuildInputs = [
      pkgs.ocamlPackages.fmt
      pkgs.ocamlPackages.prelude
      smtml
    ];
  });
in

pkgs.mkShell {
  name = "owi-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    bisect_ppx
    pkgs.framac
    landmarks
    landmarks-ppx
    pkgs.mdbook
    pkgs.mdbook-plugins
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
    ocp-browser
    ocp-index
    ocb
    odoc
    sedlex
    # unwrapped because wrapped tries to enforce a target and the build script wants to do its own thing
    pkgs.llvmPackages.clang-unwrapped
    # lld + llc isn't included in unwrapped, so we pull it in here
    pkgs.llvmPackages.bintools-unwrapped
    pkgs.tinygo
    pkgs.rustc
    pkgs.zig
    pkgs.makeWrapper
  ];
  buildInputs = with pkgs.ocamlPackages; [
    bos
    cmdliner
    crowbar
    digestif
    dolmen_type
    domainpc
    dune-build-info
    dune-site
    hc
    integers
    menhirLib
    ocamlgraph
    ocaml_intrinsics
    patricia-tree
    prelude
    processor
    scfg
    smtml
    symex
    synchronizer
    uutf
    xmlm
    yojson
    z3
    zarith
  ];
  shellHook = ''
    export PATH=$PATH:${pkgs.lib.makeBinPath [
      pkgs.llvmPackages.bintools-unwrapped
      pkgs.llvmPackages.clang-unwrapped
      pkgs.rustc
      pkgs.zig
    ]}
  '';
}
