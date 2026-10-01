FROM debian:unstable

ENV DEBIAN_FRONTEND=noninteractive
ENV HOME=/root
ENV OPAMCONFIRMLEVEL=unsafe-yes
ENV OPAMSWITCH=ocaml
ENV PATH="/root/.opam/ocaml/bin:${PATH}"

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
        https://ziglang.org/download/0.14.0/zig-linux-x86_64-0.14.0.tar.xz && \
    mkdir /tmp/zig && \
    tar xf zig-linux-x86_64-0.14.0.tar.xz \
        -C /tmp/zig \
        --strip-components 1 && \
    mv /tmp/zig/* /usr/local/bin/ && \
    rm -rf /tmp/zig zig-linux-x86_64-0.14.0.tar.xz

RUN wget -nv \
        https://github.com/tinygo-org/tinygo/releases/download/v0.40.1/tinygo_0.40.1_amd64.deb && \
    dpkg -i tinygo_0.40.1_amd64.deb > /dev/null && \
    rm tinygo_0.40.1_amd64.deb

COPY owi.opam ./

RUN opam install . --depext-only --with-test && \
    opam install . --deps-only --with-test && \
    rm ./owi.opam

RUN rm -rf /var/lib/apt/lists/*
