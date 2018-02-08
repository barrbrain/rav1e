#!/bin/bash
set -e
set -x
cd `dirname $0`
cd aom_build/aom
ORIG_HEAD=`git rev-parse HEAD`
# origin/rav1e_7b
# 22f3205cdb
# a83fbaa25c
# 110f133fed
# 3efe3e76c0
# 4736342b2c
EDITOR=true git merge 4736342b2c ||
  { git merge --abort && false; }
trap "cd $PWD; git reset --hard $ORIG_HEAD" ERR
cd ../..
cargo test --release
cargo build --release
target/release/rav1e -l 3 -r recon.y4m -o output.ivf input.y4m
rm -fr aom_test
mkdir -p aom_test
cd aom_test
../aom_build/aom/configure --enable-debug --enable-experimental --disable-unit-tests --disable-aom_qm --disable-ext-intra --disable-loop_restoration --disable-ext_partition --disable-ext_partition_types --disable-loopfilter_level --disable-intra_edge --disable-cfl --disable-kf-ctx --disable-striped_loop_restoration --disable-max_tile --disable-ext-intra-mod --disable-frame_size \
	--disable-q_adapt_probs \
	--disable-av1-encoder --enable-ccache
make -j10
./aomdec -o ../output.y4m ../output.ivf || false
cmp <(tail -n+2 ../output.y4m) <(tail -n+2 ../recon.y4m)
cd `dirname $0`/aom_build/aom
git reset --hard $ORIG_HEAD
