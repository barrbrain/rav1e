#!/bin/env python3
import collections
import fcntl
import hashlib
import lzma
import os
import selectors
import subprocess
import sys
import tarfile
from multiprocessing.dummy import Pool

output = collections.deque()


def collect_q(f, q, s):
    of = f"{f}-{q}-{s}.xz"
    cmd = f"zstd -d < {f} | target/release/rav1e -o /dev/null --threads 1 --tiles 1 -v --metrics --no-scene-detection --keyint 0 --speed 5 --quantizer {q} --strength {s} - 2>&1 | tee >(xz > {of}.part) | wc -c; mv {of}.part {of}"
    if os.path.exists(of):
        cmd = f'xz -d < {of} | wc -c'
    output.append(f'Run: {cmd}')
    txt_size = subprocess.run(cmd,
                              shell=True,
                              stdin=subprocess.DEVNULL,
                              capture_output=True).stdout
    size = int(txt_size.strip())
    output.append(f'End: {cmd}')
    return (of, size)


pool = Pool(64)

jobs = []


def enqueue(q, s):
    tasks = [(f, q, s) for f in sys.argv[1:]]
    h = hashlib.blake2b(digest_size=20)
    for t in tasks:
        h.update('\0'.join(t).encode('utf-8'))
    tag = h.hexdigest()
    done = os.path.exists(tag + '.tar.xz')
    if not done:
        jobs.append((tag, pool.starmap_async(collect_q, tasks)))
    output.append(f'Queued: {tag} {q} {s}')
    if done:
        output.append(f'Ready: {tag}')


# Non-blocking input on stdin
flags = fcntl.fcntl(sys.stdin, fcntl.F_GETFL) | os.O_NONBLOCK
fcntl.fcntl(sys.stdin, fcntl.F_SETFL, flags)

# Non-blocking input on stdout
flags = fcntl.fcntl(sys.stdout, fcntl.F_GETFL) | os.O_NONBLOCK
fcntl.fcntl(sys.stdout, fcntl.F_SETFL, flags)

terminate = False


def parse(stdin):
    global terminate
    for line in stdin.read().splitlines():
        if line == 'TERM':
            terminate = True
            pool.close()
        else:
            args = line.split()
            if len(args) == 2:
                enqueue(*args)


def multiplex(stdout):
    while output:
        line = output.popleft()
        try:
            stdout.write(line + '\n')
            stdout.flush()
        except:
            output.appendleft(line)
            break


sel = selectors.DefaultSelector()
try:
    sel.register(sys.stdin, selectors.EVENT_READ, parse)
except:
    parse(sys.stdin)
sel.register(sys.stdout, selectors.EVENT_WRITE, multiplex)

while output or jobs or not terminate:
    ready = [j for j in jobs if j[1].ready()]
    for f, result in ready:
        name = f + ".tar.xz"
        with tarfile.open(name + '.part', "w:xz") as tar:
            for (tmp, ti_size) in result.get():
                ti = tar.gettarinfo(tmp, arcname=tmp[:-len('.xz')])
                ti.size = ti_size
                with lzma.open(tmp, format=lzma.FORMAT_XZ) as t:
                    tar.addfile(ti, t)
                os.unlink(tmp)
        os.rename(name + '.part', name)
        output.append(f'Ready: {f}')
    if ready:
        jobs = [j for j in jobs if j not in ready]
    else:
        for k, mask in sel.select(1):
            k.data(k.fileobj)

pool.join()
