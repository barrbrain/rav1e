FROM gcr.io/oss-fuzz-base/base-builder-rust
RUN apt-get update && apt-get install --no-install-recommends -y \
        clang curl llvm-dev libclang-dev pkg-config \
	python3-pip python3-setuptools python3-wheel nasm && \
    pip3 install meson ninja
RUN git clone --depth 1 -b 1.0.0 https://code.videolan.org/videolan/dav1d.git dav1d
COPY . $SRC/rav1e
WORKDIR rav1e
COPY .clusterfuzzlite/build.sh .clusterfuzzlite/*.options $SRC/
