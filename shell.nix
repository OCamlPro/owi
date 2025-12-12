{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  smtml = pkgs.ocamlPackages.smtml.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "redianthus";
      repo = "smtml";
      rev = "b8853d3a589722652904dfc1d6f85958f954dd4c";
      hash = "sha256-9ogUDfsUbHsEfMhxdfA/bvUVLy4oxBT7pXnGQkPiLkY=";
    };
  });
  synchronizer = pkgs.ocamlPackages.synchronizer.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "synchronizer";
      rev = "e70f51f7518ac2bfa02243ec3820fb16dc7f9ad9";
      hash = "sha256-g1JtiWSeco2ULjE0wtJGM8ZvyJFSnyj+3gjTakYF8Uc=";
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
