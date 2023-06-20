#!/bin/env python3
import collections
import selectors
import sys
import tarfile

from paramiko.client import SSHClient

import search

cwd = '/home/barrbrain/rav1e'
hosts = ['localhost'] * 2

clients = []
pending = dict()
running = set()
new_points = []
sinks = []
sources = []

sel = selectors.DefaultSelector()


def multiplex(stdin, new_points):
    while new_points:
        point = new_points.popleft()
        try:
            if point is None:
                stdin.write('TERM\n')
            else:
                q, s = point
                stdin.write(f'{q} {s}\n')
            stdin.flush()
        except:
            new_points.appendleft(point)
            break


def parse(channel):
    stdout = [s for s in sources if s.channel == channel][0]
    while not stdout.closed:
        try:
            line = stdout.readline()
        except:
            break
        if not line:
            sel.unregister(stdout.channel)
            stdout.close()
            break
        if line.startswith('Queued: '):
            _, tag, q, s = line.rstrip().split()
            q, s = int(q), float(s)
            pending[tag] = (q, s)
        elif line.startswith('Ready: '):
            _, tag = line.rstrip().split()
            with channel.transport.open_sftp_client() as sftp:
                with sftp.file(f'{cwd}/{tag}.tar.xz') as f:
                    with tarfile.open(fileobj=f, mode='r:xz') as tar:
                        results = [
                            search.reduce_log(member.name,
                                              tar.extractfile(member))
                            for member in tar.getmembers()
                        ]
            q, s = pending[tag]
            del pending[tag]
            search.progress(q, s, results)
        elif line.startswith('Run: '):
            _, cmd = line.rstrip().split(maxsplit=1)
            running.add(cmd)
        elif line.startswith('End: '):
            _, cmd = line.rstrip().split(maxsplit=1)
            running.discard(cmd)


def connect():
    for host, path in zip(
            hosts,
            'green_grass_1920x1080_30-868-1036.y4m.zst hard_rock_1920x1080_25-405-500.y4m.zst'
            .split()):
        client = SSHClient()
        client.load_system_host_keys()
        client.connect(host)
        stdin, stdout, stderr = client.exec_command(
            f'cd {cwd}; ./driver.py {path}', timeout=0)
        sinks.append(stdin)
        sources.append(stdout)
        new_points.append(collections.deque())
        sel.register(stdout.channel, selectors.EVENT_READ, parse)


def enqueue(params):
    for points in new_points:
        points.extend(params)


def main():
    connect()
    search.open(enqueue)
    while any(not s.closed for s in sources):
        for args in zip(sinks, new_points):
            multiplex(*args)
        for k, mask in sel.select(1):
            k.data(k.fileobj)
    search.close()


if __name__ == "__main__":
    main()
