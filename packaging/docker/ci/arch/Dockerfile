FROM archlinux:latest

ENV HOME=/root
ENV OPAMCONFIRMLEVEL=unsafe-yes
ENV PATH="/root/.opam/ocaml/bin:${PATH}"

RUN pacman -Syu --noconfirm && pacman -S --noconfirm git base-devel opam wget

WORKDIR /root

RUN opam init --disable-sandboxing --bare -y && \
    opam switch create ocaml ocaml-base-compiler \
        --criteria="-notuptodate" && \
    opam update

COPY owi.opam ./

RUN opam install . --depext-only --with-test && \
    opam install . --deps-only --with-test && \
    rm ./owi.opam
