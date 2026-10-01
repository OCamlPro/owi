FROM kumikode/owi-ci:alpine-latest

RUN apk add zlib-static gmp-static cmake ninja python3


RUN set -eux; \
    cd /tmp; \
    git clone --depth 1 https://github.com/Z3Prover/z3.git; \
    cd z3; \
    cmake -S . -B build \
        -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=OFF \
        -DCMAKE_INSTALL_PREFIX=/usr; \
    cmake --build build --parallel "$(nproc)"; \
    cmake --install build; \
    rm -rf /tmp/z3
