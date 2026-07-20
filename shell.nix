{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  bootstrapGhcWasm = pkgs.fetchurl {
    url = "https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh";
    sha256 = "sha256-ma3MaYD8TWhTGb/Sohivp08GJMYR/2Nqt5ymuX7cvJc=";
  };
  ocamlPackages = pkgs.ocamlPackages.overrideScope (self: super: {
    ocaml = (super.ocaml.overrideAttrs {
      doCheck = false;
    }).override {
      #aflSupport = true;
      #flambdaSupport = true;
      #framePointerSupport = true;
      #spaceTimeSupport = true;
    };
    landmarks = super.landmarks.overrideAttrs (old: {
      src = fetchGit {
        url = "https://github.com/hra687261/landmarks";
        rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
      };
    });
    landmarks-ppx = super.landmarks-ppx.overrideAttrs (old: {
      src = fetchGit {
        url = "https://github.com/hra687261/landmarks";
        rev = "17be3567a63650090f9cf94654fcc8d99f946e27";
      };
      meta.broken = false;
    });
    smtml = super.smtml.overrideAttrs (old: {
      src = fetchGit {
        url = "https://github.com/formalsec/smtml";
        rev = "079c2cff225ff42d4bdb51ba1ff591c52034a896";
      };
    });
    symex = super.symex.overrideAttrs (old: {
      src = fetchGit {
        url = "https://github.com/ocamlpro/symex";
        rev = "3885298408df002085a180d2c7eae25d99e7262c";
      };
    });
  });
  tinygo = pkgs.tinygo.overrideAttrs (old: {
    doCheck = false;
  });

  codex = ocamlPackages.buildDunePackage (finalAttrs: {
    dontDetectOcamlConflicts = true;
    pname = "codex";
    version = "dev";
    src = pkgs.fetchFromGitHub {
      owner = "redianthus";
      repo = "codex";
      rev = "bda2db6d8c6271b26edf948d24b09860037d4761";
      hash = "sha256-7jeIN+U7lGfwWzWc55SMXEgKWyDt8GAPbmZo+fxcA9Q=";
    };

    nativeBuildInputs = with ocamlPackages; [
      dune_3
      findlib
      js_of_ocaml
      # tests
      mdx
      mdx.bin
      qcheck-core
    ];
    buildInputs = with ocamlPackages; [
      js_of_ocaml
      js_of_ocaml-ppx
      ppx_deriving
      ppx_inline_test
    ];
    propagatedBuildInputs = with ocamlPackages; [
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
    # landmarks
    # landmarks-ppx
    pkgs.mdbook
    pkgs.mdbook-plugins
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
    #ocaml-lsp
    ocp-browser
    ocp-index
    ocb
    odoc
    sedlex
    pkgs.curl
    pkgs.git
    pkgs.jq
    # lld + llc isn't included in unwrapped, so we pull it in here
    pkgs.llvmPackages.bintools-unwrapped
    # unwrapped because wrapped tries to enforce a target and the build script wants to do its own thing
    pkgs.llvmPackages.clang-unwrapped
    tinygo
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

    # uncomment if you want Haskell support
    #if [ ! -f "$HOME/.ghc-wasm/env" ]; then
    #  echo "Running ghc-wasm bootstrap..."
    #  bash "${bootstrapGhcWasm}"
    #fi
    #
    #source "$HOME/.ghc-wasm/env"
  '';
}
