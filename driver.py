#!/bin/env python3
import lzma
import os
from pprint import pprint
import shutil
import subprocess
import sys
import time
from multiprocessing.dummy import Pool

def collect_q(s, f, q):
  of = f"{f}-{s}-{q}.xz"
  cmd = f"zstd -d < {f} | target/release/rav1e -o /dev/null --threads 1 --tiles 1 -q --no-scene-detection --keyint 0 --speed {s} --quantizer {q} - | xz > {of}"
  print(cmd)
  subprocess.run(cmd, shell=True)
  return of

pool = Pool(64)

jobs = []

for f in sys.argv[1:]:
  tasks = [(s, f, q) for s in [5, 8] for q in [78, 98, 118, 138, 158, 188]]
  jobs.append((f, pool.starmap_async(collect_q, tasks)))

while jobs:
  ready = [j for j in jobs if j[1].ready()]
  for f, result in ready:
    with lzma.open(f + ".txt.xz", "wb") as f:
      for tmp in result.get():
        with lzma.open(tmp) as t:
          shutil.copyfileobj(t, f)
        os.unlink(tmp)
  if ready:
    jobs = [j for j in jobs if j not in ready]
  else:
    time.sleep(30)
