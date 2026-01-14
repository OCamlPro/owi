{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  domainpc = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {

    pname = "domainpc";
    version = "0.19.0";

    src = pkgs.fetchFromGitHub {
      owner = "redianthus";
      repo = "domainpc";
      rev = "289d4940a8f95f31defb3594be28f44749ff6afd";
      hash = "sha256-v5FpsHvXCLKEpn0c2gEBGK15yPJQE5ZzwSq5foXv8VE=";
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
