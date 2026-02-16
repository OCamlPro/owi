{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  domainpc = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {

    pname = "domainpc";
    version = "0.2";

    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "domainpc";
      rev = "2bc85ccfa008910afc0f511919558e827320bbf2";
      hash = "sha256-VyCbxVikV0+YZzgC/8i4RLxVWN3TMS6n0qR72SmVwI8=";
    };

    propagatedBuildInputs = [
      pkgs.ocamlPackages.processor
    ];
  });
  synchronizer = pkgs.ocamlPackages.synchronizer.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "synchronizer";
      rev = "4a12eb003620e249176e2046cc0dd76dbd8ff733";
      hash = "sha256-CKpWGSXV7oXFVWEKrM4nlmSHFRt9et/QYOq97az1vt4=";
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
