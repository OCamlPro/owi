## unreleased

- add a basic optimization mode
- add a profiling mode
- improve compilation time by using menhir with table mode
- improve performance (ifprintf was critical in the interpreter loop)
- allow to name linked module (no need to register them anymore)
- make `spectec` print stuff for real
- fix bug with memory grow limit on i32
- fix bug with i32 to int conversion
- fix bug with address on i32 architecture
- fix `menhir` lower bound

## 0.1 - 2023-01-14

- first release
