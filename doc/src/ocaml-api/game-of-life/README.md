# Conway's Game of Life

This example is a [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) implementation.
3 outputs are proposed (console, [OCaml Graphics](https://github.com/ocaml/graphics) and HTML/JavaScript),
each one using the same WebAssembly modules.

## Console and OCaml Graphics

Console and Graphics implementations use *external functions* technique as the
[standard example](../custom-functions.md). Here are the command lines to run in this directory to execute the programs:

* Console  mode: `dune exec -- ./life_graphics.exe`
* Graphics mode: `dune exec -- ./life_console.exe`

Several modules (defined in `life.wat` and `life_loop.wat` files) are linked.

**Why are two `.wat` files required ?**

Because, in JavaScript, the `Sleep` function uses the
[Promise system](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
which cannot work in a wasm call context.
So, if we want to use the same WebAssembly components in all contexts,
the game loop had to be extracted in a separate module.

See the following section for JavaScript implementation details.

## HTML/JavaScript

Here is a standard example of a wasm module called in a web page using the
[JavaScript API](https://webassembly.org/getting-started/js-api/).

First of all, a compiled version of the WebAssembly module is needed, obtained by the
command line `wat2wasm life.wat`. wat2wasm is part of the
[WABT: The WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt).

As in OCaml in the previous contexts, external functions are defined in the environment execution
(here in JavaScript). As explained before, only one wasm module is imported (`life.wasm`) and
the game loop has been written directly in JavaScript.
This function (`life()`) must be `async` because `sleep` function is called in `await` mode.

To see the final result, simply open `life.html` in your favorite browser.

However, the command line `dune exec -- ./runweb.exe` launches a full build and open it
directly in your browser. Some additional dependencies are required including
[ocaml-cohttp](https://github.com/mirage/ocaml-cohttp) and
[ocaml-crunch](https://github.com/mirage/ocaml-crunch).
