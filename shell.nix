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

  # TODO: remove this (and the corresponding variable set in deploy CI) once mdbooktabs is part of nixpkgs and don't make everything so slow...
  enableMdbookTabs = builtins.getEnv "MDBOOK_TABS" != "";
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
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
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
  ] ++ pkgs.lib.optional enableMdbookTabs mdbookTabs;
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
