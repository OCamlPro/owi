let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
  smtml = ocamlPackages.smtml.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "formalsec";
      repo = "smtml";
      rev = "27aa552f228cdaf36283396ade722cdcae5d1712";
      hash = "sha256-VnkF+bZXeqaj9LSpyzqH5AM9EQsrW4Rlj5kvyTfYTKE=";
    };
    doCheck = false;
  });
  synchronizer = ocamlPackages.synchronizer.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "ocamlpro";
      repo = "synchronizer";
      rev = "cb5fd995e8a42e5244acf68f238221594fd19a8d";
      hash = "sha256-0XtPHpDlyH1h8W2ZlRvJbZjCN9WP5mzk2N01WFd8eLQ=";
    };
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
  buildInputs = with ocamlPackages; [
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
