<div align="center">
  <a href="https://ocamlpro.github.io/owi">
    <img src="doc/src/logo.png" width=300 alt="Owi logo"/>
  </a>
  <br />
  <strong>Owi: Seamless bug-finding for Wasm, C, C++, Rust and Zig</strong>
</div>

<div align="center">
<br />

[![build-badge]][build status] [![coverage-badge]][code coverage] [📘 documentation] [💬 zulip]

</div>

<hr />

**Owi** is an automatic bug-finding tool for C, C++, Go, Rust and Zig. It can also be used for test-case generation, proof of programs and solver-aided programming. It works at the WebAssembly level, and thus incidentally provides a Wasm Swiss Army Knife: a formatter, an interpreter, a validator, a converter between `.wasm` and `.wat`, but also a fuzzer! Owi being written in OCaml, you can also use it as an OCaml library for many purposes.

### Key resources

<div>
  📘 <kbd><a href="https://ocamlpro.github.io/owi">Documentation</a></kbd> → Read the documentation<br />
  💬 <kbd><a href="https://owi.zulipchat.com">Zulip community</a></kbd> → Ask questions and collaborate<br />
</div>

### License

    Owi
    Copyright (C) 2021-2024 OCamlPro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

See [LICENSE].

A few files have been taken from the Wasm reference interpreter. They are licensed under the Apache License 2.0 and have a different copyright which is stated in the header of the files.

Some code has been taken from the `base` library from Jane Street. It is licensed under the MIT License and have a different copyright which is stated in the header of the files.

Some code has been taken from the E-ACSL plugin of Frama-C. It is licensed under the GNU Lesser General Public License 2.1 and have a different copyright which is stated in the header of the files.

[LICENSE]: ./LICENSE.md

[build-badge]: https://github.com/OCamlPro/owi/actions/workflows/build-nix.yml/badge.svg
[build status]: https://github.com/ocamlpro/owi/actions
[coverage-badge]: https://raw.githubusercontent.com/ocamlpro/owi/gh-pages/coverage/badge.svg
[code coverage]: https://ocamlpro.github.io/owi/coverage
[📘 documentation]: https://ocamlpro.github.io/owi
[💬 zulip]: https://owi.zulipchat.com
