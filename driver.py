#!/bin/env python3
from pprint import pprint
import numpy as np
import subprocess
import sys

def neg_avg_psnr(strength, filename, q):
  cmd = f"zstd -d < {filename} | target/release/rav1e -o /dev/null -s5 --threads 1 --tiles 1 -v --psnr --no-scene-detection --keyint 0 --strength {strength[0]} - --quantizer {q}"
  print(cmd)
  with subprocess.Popen(cmd, stderr=subprocess.PIPE, shell=True) as proc:
    for line in proc.stderr:
      # >  encoded 60 frames, 6.064 fps, 37.86 Kb/s, elapsed: 9s
      if b'encoded' in line and b'Input Frame' not in line:
        print(line.decode().rstrip())
        rate = float(line.split()[7].decode())
      # >  Mean PSNR: Avg: 41.4705  Y: 40.2983  Cb: 45.6217  Cr: 45.7722
      if b'Mean PSNR' in line:
        avg = float(line.split()[5].decode())
        print(line.decode().rstrip())
        return (avg, rate, q, *strength)
  return 0

with open(sys.argv[1] + ".csv", "w") as f:
  # 78 98 118 138 158 188
  for q in [188, 158, 138, 118, 98, 78]:
    args = (sys.argv[1], q)
    full_scan = np.linspace(1.5, 4.5, 31)
    for x in full_scan:
      v = neg_avg_psnr([x], *args)
      f.write(",".join(map(str, v)) + "\n")
