FROM fedora:latest

ENV HOME=/root
ENV OPAMCONFIRMLEVEL=unsafe-yes
ENV PATH="/root/.opam/ocaml/bin:${PATH}"

RUN dnf update -y && dnf install -y ca-certificates sed gzip grep git wget tar xz opam

WORKDIR /root

RUN opam init --disable-sandboxing --bare -y && \
    opam switch create ocaml ocaml-base-compiler \
        --criteria="-notuptodate" && \
    opam update

COPY owi.opam ./

RUN opam install . --depext-only --with-test && \
    opam install . --deps-only --with-test && \
    rm ./owi.opam
