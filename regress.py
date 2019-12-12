from io import BytesIO
import numpy as np
import sys

# Klotz, Jerome H. "UPDATING SIMPLE LINEAR REGRESSION."
# Statistica Sinica 5, no. 1 (1995): 399-403.
# http://www.jstor.org/stable/24305577
def online_simple_regression(accumulator, x, y):
    Ax_, Ay_, Sxy, Sxx, n_, minx, maxx = accumulator or (0, 0, 0, 0, 0, None, None)

    first = n_ == 0
    n = n_ + x.size
    rt_n, rt_n_ = np.sqrt((n, n_), dtype=np.float128)

    Ax = (Ax_*n_ + x.sum(dtype=np.float128))/n
    Ay = (Ay_*n_ + y.sum(dtype=np.float128))/n
    
    minx = x.min() if first else min(minx, x.min())
    maxx = x.max() if first else max(maxx, x.max())
    
    X = Ax if first else (Ax_*rt_n_ + Ax*rt_n)/(rt_n_ + rt_n)
    Y = Ay if first else (Ay_*rt_n_ + Ay*rt_n)/(rt_n_ + rt_n)

    Sxx += np.sum((x - X)**2)
    Sxy += np.sum((x - X)*(y - Y))

    return Ax, Ay, Sxy, Sxx, n, minx, maxx

acc = None
while True:
    # data = np.loadtxt(sys.stdin, dtype=np.float64, max_rows=1024*1024)
    raw = sys.stdin.buffer.read(8*2*1024*1024)
    data = np.reshape(np.frombuffer(raw, dtype=np.float64), (-1, 2))
    if data.size == 0: break
    acc = online_simple_regression(acc, data.T[0], data.T[1])
    Ax, Ay, Sxy, Sxx, n, minx, maxx = acc
    beta = Sxy / Sxx
    alpha = Ay - Ax * beta
    print(f'{beta}\tx + {alpha}\t{n:,}')
