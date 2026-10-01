FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive
ENV HOME=/root
ENV RUSTUP_HOME=/root/.rustup
ENV CARGO_HOME=/root/.cargo
ENV OPAMCONFIRMLEVEL=unsafe-yes
ENV OPAMSWITCH=ocaml
ENV PATH="/root/.cargo/bin:/root/.opam/ocaml/bin:${PATH}"

RUN apt update -yqq && \
    apt install -yqq \
        ca-certificates \
        git \
        opam \
        wget \
        xz-utils

WORKDIR /root

RUN opam init --disable-sandboxing --bare -y && \
    opam switch create ocaml ocaml-base-compiler \
        --criteria="-notuptodate" && \
    opam update

RUN wget -nv \
        https://github.com/tinygo-org/tinygo/releases/download/v0.40.1/tinygo_0.40.1_amd64.deb && \
    dpkg -i tinygo_0.40.1_amd64.deb > /dev/null && \
    rm tinygo_0.40.1_amd64.deb

COPY owi.opam ./

RUN opam install . --depext-only --with-test && \
    opam install . --deps-only --with-test && \
    rm ./owi.opam

RUN rm -rf /var/lib/apt/lists/*

RUN rustup default stable && rustup target add wasm32-unknown-unknown
