FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive
ARG COGUTIL_REF=master

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    cmake \
    git \
    pkg-config \
    libboost-all-dev \
    libgmp-dev \
    guile-3.0-dev \
    cxxtest \
    cython3 \
    libpython3-dev \
    python3-pytest \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp
RUN git clone --depth 1 --branch "${COGUTIL_REF}" \
    https://github.com/opencog/cogutil.git \
    cogutil

RUN cmake -S /tmp/cogutil -B /tmp/cogutil-build \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    && cmake --build /tmp/cogutil-build --parallel \
    && cmake --install /tmp/cogutil-build \
    && rm -rf /tmp/cogutil /tmp/cogutil-build

WORKDIR /workspace
CMD ["bash"]
