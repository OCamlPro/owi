# Coding Guidelines

## The `prelude` library

We use the [`prelude`](https://git.zapashcanon.fr/zapashcanon/prelude) library to **hide dangerous functions** from the standard library.
It is automatically opened in the whole project.
More than dangerous functions, this library also hide some modules for which better alternatives exists.
For instance, all system interactions are done using [`Bos`](https://erratique.ch/software/bos/doc/) and all the formatting is done with [`Fmt`](https://erratique.ch/software/fmt/doc/).

## Printing

Read the [Logs basics](https://erratique.ch/software/logs/doc/Logs/index.html#basics) and in particular, the [usage conventions](https://erratique.ch/software/logs/doc/Logs/index.html#usage).
