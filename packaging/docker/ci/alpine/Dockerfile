FROM alpine:latest

ENV HOME=/root
ENV OPAMCONFIRMLEVEL=unsafe-yes
ENV OPAMSWITCH=ocaml
ENV PATH="/root/.opam/ocaml/bin:${PATH}"

RUN apk add --no-cache \
        bash \
        build-base \
        ca-certificates \
        # clang shouldn't be needed but otherwise it's not added by the depext for some reason... it's weird
        clang \
        curl \
        git \
        opam \
        tar \
        xz

WORKDIR /root

RUN opam init --disable-sandboxing --bare -y && \
    opam switch create ocaml ocaml-base-compiler \
        --criteria="-notuptodate" && \
    opam update

RUN wget -nv \
        https://github.com/tinygo-org/tinygo/releases/download/v0.40.1/tinygo0.40.1.linux-amd64.tar.gz && \
    tar -xzf tinygo0.40.1.linux-amd64.tar.gz \
        -C /usr/local && \
    ln -s /usr/local/tinygo/bin/tinygo /usr/local/bin/tinygo && \
    rm tinygo0.40.1.linux-amd64.tar.gz

COPY owi.opam ./

RUN opam install . --depext-only --with-test && \
    opam install . --deps-only --with-test && \
    rm ./owi.opam
