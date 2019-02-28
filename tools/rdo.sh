#!/bin/bash
set -e
rm -f rdo.dat
cargo build --release
ls subset1-y4m/*.y4m | parallel target/release/rav1e --quantizer {2} -o /dev/null --train-rdo {1} :::: - ::: 16 48 80 95 112 144 176 208 240
#gnuplot rdo.plt -p
