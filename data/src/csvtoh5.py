#!/bin/env python3
import csv
import sys
import h5py
import numpy as np
import matplotlib.tri as tri

filename = sys.argv[1]
rep = sys.argv[2]
td = sys.argv[3]
ed = sys.argv[4]
numpoints = int(sys.argv[5])
xcoords = []
ycoords = []
runtimes = []
with open(filename,'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        if rep != row['representation']:
            continue
        if ed != row['representation.extra_dimensions']:
            continue
        if td != row['representation.target_distortion']:
            continue

        xcoords.append(float(row['x']))
        ycoords.append(float(row['y']))
        runtimes.append(float(row['runtime']))
x = np.array(xcoords)
y = np.array(ycoords)
z = np.array(runtimes)
xi = np.linspace(min(x)-0.1,max(x)+0.1, numpoints)
yi = np.linspace(min(y)-0.1, max(y)+0.1, numpoints)
triang = tri.Triangulation(x, y)
interpolator = tri.LinearTriInterpolator(triang, z)
Xi, Yi = np.meshgrid(xi, yi)
zi = interpolator(xi, yi)
Zi = interpolator(Xi, Yi)
print(np.amin(Zi))
print(np.amax(Zi))

output = filename.replace('.csv','.h5')
with h5py.File(output,'w') as f:
    f['xcoordinates'] = xi
    f['ycoordinates'] = yi
    f['runtime'] = Zi

#python src/loss-landscape/h52vtp.py -f coolidge_multiple_metrics.projection.h5 --surf_name runtime

