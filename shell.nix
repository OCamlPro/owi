{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
  landmarks = ocamlPackages.landmarks.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "hra687261";
      repo = "landmarks";
      rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
      hash = "sha256-3ui4uvSAvUgzk2UMVtH9A4BhAX6nWbwx7q0YwkANNv8=";
    };
  });
  landmarks-ppx = ocamlPackages.landmarks-ppx.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "hra687261";
      repo = "landmarks";
      rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
      hash = "sha256-3ui4uvSAvUgzk2UMVtH9A4BhAX6nWbwx7q0YwkANNv8=";
    };
    meta.broken = false;
  });

  codex = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "codex";
    version = "dev";
    src = pkgs.fetchFromGitHub {
      owner = "redianthus";
      repo = "codex";
      rev = "ca0d9f6092ee577e445c062a7a944badd956703e";
      hash = "sha256-+R8IqZXPkkFKGA6vtPT92RJWHVreKZPS9oJ0YnRUprs=";
    };

    nativeBuildInputs = with pkgs.ocamlPackages; [
      dune_3
      findlib
      js_of_ocaml
      # tests
      mdx
      mdx.bin
      qcheck-core
    ];
    buildInputs = with pkgs.ocamlPackages; [
      js_of_ocaml
      js_of_ocaml-ppx
      ppx_deriving
      ppx_inline_test
    ];
    propagatedBuildInputs = with pkgs.ocamlPackages; [
      base64
      bheap
      camlp-streams
      cudd
      fmt
      pacomb
      patricia-tree
      ppx_inline_test
      vdom
      zarith
    ];


    tailwindFile = pkgs.fetchurl {
      url = "https://github.com/codex-semantics-library/codex/releases/download/1.0-rc4/tailwind4.1.5.css";
      hash = "sha256-HgE6gWIf2+0W908sYIjhUDYkUnsIz0mJN2nHqXY3QD8=";
    };
    graphviz = pkgs.fetchurl {
      url = "https://github.com/codex-semantics-library/codex/releases/download/1.0-rc4/graphviz.umd.js";
      hash = "sha256-JeT1R2S8FhCSAcL0zsJjx7ai+bL1X3AjHcgEniwm33c=";
    };
    bundleOutput = pkgs.fetchurl {
      url = "https://github.com/codex-semantics-library/codex/releases/download/1.0-rc4/bundle-output.js";
      hash = "sha256-zYKJDdu5fkZ52eEzDBuk6mSyxzMDMWDatK/ynlpjU2o=";
    };

    postUnpack = ''
      mkdir -p utils/gui/deps/js
      cp ${finalAttrs.tailwindFile} $sourceRoot/utils/gui/deps/tailwind4.1.5.css
      cp ${finalAttrs.graphviz} $sourceRoot/utils/gui/deps/graphviz.umd.js
      cp ${finalAttrs.bundleOutput} $sourceRoot/utils/gui/deps/js/bundle-output.js
    '';
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
    codex
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
