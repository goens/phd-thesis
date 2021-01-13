#!/bin/env python3
import csv
import sys
import h5py
import numpy as np

filename = sys.argv[1]
rep = sys.argv[2]
td = sys.argv[3]
ed = sys.argv[4]
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
xcoords = np.array(xcoords)
ycoords = np.array(ycoords)
runtimes = np.array(runtimes)
output = filename.replace('.csv','.h5')
with h5py.File(output,'w') as f:
    f['xcoordinates'] = xcoords
    f['ycoordinates'] = ycoords
    f['runtime'] = runtimes


#python src/loss-landscape/h52vtp.py -f coolidge_multiple_metrics.projection.h5 --surf_name runtime

