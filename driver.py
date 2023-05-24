#!/bin/env python3
from pprint import pprint
import numpy as np
import subprocess
import sys

def neg_avg_psnr(strength, filename, q):
  cmd = "target/debug/rav1e -o /dev/null -s8 --tiles 4 -v --tune Psnr --psnr --strength".split() + \
    [str(*strength), filename, '--quantizer', str(q)]
  print(' '.join(cmd))
  if strength[0] < 1.0: return 0
  if strength[0] > 3.0: return 0
  with subprocess.Popen(cmd, stderr=subprocess.PIPE) as proc:
    for line in proc.stderr:
      # >  encoded 60 frames, 6.064 fps, 37.86 Kb/s, elapsed: 9s
      if b'encoded 60 frames' in line and b'Input Frame' not in line:
        print(line.decode().rstrip())
        rate = float(line.split()[7].decode())
      # >  Mean PSNR: Avg: 41.4705  Y: 40.2983  Cb: 45.6217  Cr: 45.7722
      if b'Mean PSNR' in line:
        avg = float(line.split()[5].decode())
        print(line.decode().rstrip())
        return (avg, rate, q, *strength)
  return 0

with open(sys.argv[1] + ".csv", "w") as f:
  for q in [252, 220, 172, 128, 80]:
    args = (sys.argv[1], q)
    full_scan = np.linspace(1.0, 3.0, 21)
    for x in full_scan:
      v = neg_avg_psnr([x], *args)
      f.write(",".join(map(str, v)) + "\n")
