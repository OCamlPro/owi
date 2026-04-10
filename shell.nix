{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4.overrideScope (self: super: {
    landmarks = super.landmarks.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "hra687261";
        repo = "landmarks";
        rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
        hash = "sha256-3ui4uvSAvUgzk2UMVtH9A4BhAX6nWbwx7q0YwkANNv8=";
      };
    });
    landmarks-ppx = super.landmarks-ppx.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "hra687261";
        repo = "landmarks";
        rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
        hash = "sha256-3ui4uvSAvUgzk2UMVtH9A4BhAX6nWbwx7q0YwkANNv8=";
      };
      meta.broken = false;
    });
    smtml = super.smtml.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "formalsec";
        repo = "smtml";
        rev = "3d19685d6859df7695eb697571b2a8ec41638068";
        hash = "sha256-dWZrN0hTxxqGC2queit91GDuw/x5fyRPwHbmKxkvc/w=";
      };
    });
  });
  tinygo = pkgs.tinygo.overrideAttrs (old: {
    doCheck = false;
  });
in

pkgs.mkShell {
  name = "owi-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with ocamlPackages; [
    dune_3
    findlib
    bisect_ppx
    pkgs.framac
    # landmarks
    # landmarks-ppx
    pkgs.mdbook
    pkgs.mdbook-plugins
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
    ocaml-lsp
    ocp-browser
    ocp-index
    ocb
    odoc
    sedlex
    # unwrapped because wrapped tries to enforce a target and the build script wants to do its own thing
    pkgs.llvmPackages.clang-unwrapped
    # lld + llc isn't included in unwrapped, so we pull it in here
    pkgs.llvmPackages.bintools-unwrapped
    tinygo
    pkgs.rustc
    pkgs.zig
    pkgs.makeWrapper
  ];
  buildInputs = with ocamlPackages; [
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
