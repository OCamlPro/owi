# Installation

## Install from your package manager

Check on [repology] if Owi is available in your package manager.

### NixOS

```shell-session
$ nix-shell -p owi
```

## Install the static binary

Pre-built static binaries are available for Linux amd64 (musl).
They are standalone binaries and do not require an OCaml installation or any shared library.
The latest version can be downloaded [from the nightly release page](https://github.com/ocamlpro/owi/releases/tag/nightly).
Once you downloaded the archive:

```shell-session
$ tar -xzf owi-linux-x86_64-musl.tar.gz
```

You can install it system-wide with:

```shell-session
$ sudo cp -r bin lib share doc /usr/local
```

## Install from sources

`owi` can be installed with [opam]:

```shell-session
$ opam install owi
# if you intend to use symbolic execution you must install one solver
# you can choose any solver supported by smtml
# z3, colibri2, bitwuzla-cxx or cvc5 for instance
$ opam install z3
```

If you don't have `opam`, you can install it following the [how to install opam] guide.

If you can't or don't want to use `opam`, you can build the package with `dune build -p owi @install` but you'll first have to install the dependencies by yourself.
You can find the list of dependencies in the `dune-project` file.

### Development version

To get the development version:

```shell-session
$ git clone git@github.com:OCamlPro/owi.git
$ cd owi
$ opam install . --deps-only
$ dune build -p owi @install
$ dune install
```

[how to install opam]: https://opam.ocaml.org/doc/Install.html
[opam]: https://opam.ocaml.org
[repology]: https://repology.org/project/ocaml%3Aowi/versions
