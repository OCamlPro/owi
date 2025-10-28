{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3.overrideScope (self: super: {
    ocb = self.buildDunePackage {
      pname = "ocb";
      version = "master";
      src = pkgs.fetchgit {
        url = "https://github.com/ocamlpro/ocb.git";
        hash = "sha256-IuVP1moJpnbctYGv7X1aKq0elUQ/Zmn81MD/zBPqwno=";
      };
      propagatedNativeBuildInputs = with ocamlPackages; [
        dune_3
        findlib
        ocaml
      ];
      propagatedBuildInputs = with ocamlPackages; [
        cmdliner
        fmt
        prelude
      ];
    };
  });

  mdbookTabs = pkgs.rustPlatform.buildRustPackage rec {
    pname = "cargo-mdbook-tabs";
    version = "v0.2.3";
    src = pkgs.fetchFromGitHub {
      owner = "RustForWeb";
      repo = "mdbook-plugins";
      rev = version;
      hash = "sha256-IyIUJH5pbuvDarQf7yvrStMIb5HdimudYF+Tq/+OtvY=";
    };
    cargoHash = "sha256-/UM85Lhq52MFTjczPRuXENPJOQkjiHLWGPPW/VD9kBQ=";
    nativeBuildInputs = with pkgs; [
      perl
    ];
  };
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
    mdbookTabs
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
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
