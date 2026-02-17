{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4.overrideScope (self: super: {
    smtml = super.smtml.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "formalsec";
        repo = "smtml";
        rev = "a9dff52e7ef2215c786ee8ce2c24d716db0b5ace";
        hash = "sha256-TIOOE/bsis6oYV3Dt6TcI/r/aN3S1MQNtxDAnvBbVO0=";
      };
      doCheck = false;
    });
    symex = super.symex.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "ocamlpro";
        repo = "symex";
        rev = "94a7b8bc2afecdf6ee0d3cd3f02a89182a6b30ea";
        hash = "sha256-dE4wqyAOeAsotQZDD9F7xJrKyTh5zI1hJuxXwUCxjx0=";
      };
    });
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
