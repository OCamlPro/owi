{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  smtml = pkgs.ocamlPackages.smtml.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "formalsec";
      repo = "smtml";
      rev = "c33fa749c00bc32bd12d5e04772f687594d059d2";
      hash = "sha256-+7jHv/DjlKDsMP00j8NsgbxhDnv9VXpbVZ/ewXqShCA=";
    };
  });
  synchronizer = pkgs.ocamlPackages.synchronizer.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "synchronizer";
      rev = "4a12eb003620e249176e2046cc0dd76dbd8ff733";
      hash = "sha256-CKpWGSXV7oXFVWEKrM4nlmSHFRt9et/QYOq97az1vt4=";
    };
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
