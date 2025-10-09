# Comparison with other tools

| Tool        | Supported languages             | Automatic | Falses positives | Falses negatives          | Bug-finding | Proof of Program | Code Coverage | Licence     | Category                |
| ----------- | ------------------------------- | --------- | ---------------- | ------------------------- | ----------- | ---------------- | ------------- | ----------- | ----------------------- |
| Owi         | C, C++, Rust, TinyGo, Wasm, Zig | Yes       | No               | Only when non terminating | Yes         | Yes              | Yes           | Free        | Symbolic Execution      |
| KLEE        | C                               | Yes       | No               | Yes                       | Yes         | No               | Yes           | Free        | Symbolic Execution      |
| Frama-C WP  | C                               | No        | No               | No                        | No          | Yes              | No            | Free        | Deductive Verification  |
| Frama-C EVA | C                               | Yes       | Yes              | No                        | Yes         | Yes              | No            | Free        | Abstract Interpretation |
| Astrée      | C, C++                          | Yes       | Yes              | No                        | Yes         | Yes              | No            | Proprietary | Abstract Interpretation |
| Mopsa       | C, Python3                      | Yes       | Yes              | No                        | Yes         | Yes              | No            | Free        | Abstract Interpretation |
