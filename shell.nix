{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3.overrideScope (self: super: {
    smtml = super.smtml.overrideAttrs (oldsmtml: {
      src = pkgs.fetchFromGitHub {
        owner = "formalsec";
        repo = "smtml";
        rev = "6f98c49bcbb6a8be36e29eaa1e2191456e67503d";
        fetchSubmodules = true;
        hash = "sha256-1/P8zQIUBy17dA8ZdrzIEy7N5LAvIykOE+7WGX/ma1M=";
      };
    });
  });
in
pkgs.mkShell {
  name = "owi-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with ocamlPackages; [
    dune_3
    findlib
    mdx
    menhir
    ocaml
    ocamlformat
    odoc
    sedlex
    tiny_httpd
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
    ocaml_intrinsics
    patricia-tree
    prelude
    processor
    scfg
    smtml
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
