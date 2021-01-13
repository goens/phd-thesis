#!/bin/env python3
import csv
import sys
import math
import numpy as np

def randomSubspace(subspaceDimension, ambientDimension):
    return np.random.normal(0, 1, size=(subspaceDimension, ambientDimension))

def project(v, subspace):
    subspaceDimension = len(subspace)
    return (1 / math.sqrt(subspaceDimension)) * subspace.dot(v)

def jlt(data, subspaceDimension):
   ambientDimension = len(data[0])
   A = randomSubspace(subspaceDimension, ambientDimension)
   return (1 / math.sqrt(subspaceDimension)) * A.dot(data.T).T

def parse_mapping(m):
    res = []
    values = m.split(', ')
    for v in values:
        res.append(float(v))
    return res


filename = sys.argv[1]

mappings = {}
runtimes = {}

with open(filename,'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        rep = row['representation']
        td = row['representation.target_distortion']
        ed = row['representation.extra_dimensions']
        if (rep,td,ed) not in mappings:
            mappings[(rep,td,ed)] = []
            runtimes[(rep,td,ed)] = []

        mapping = parse_mapping(row['mapping'])
        runtime = float(row['runtime'])
        mappings[(rep,td,ed)].append(mapping)
        runtimes[(rep,td,ed)].append(runtime)
for (rep,td,ed) in mappings:
    print(f"read mappings {len(mappings[(rep,td,ed)])} for {rep}, {td}, {ed}.")

projections = {}
for (rep,td,ed) in mappings:
    mappings_matrix = np.array(mappings[(rep,td,ed)]).reshape((len(runtimes[(rep,td,ed)]),-1))
    print(f"read mappings matrix for {rep}, {td}, {ed}: (shape {mappings_matrix.shape})")
    projection = jlt(mappings_matrix,2)
    print(f"projected onto 2 dimensions (with {len(np.unique(projection))} unique entries)")
    projections[(rep,td,ed)] = projection

output_filename = filename.replace('.csv', '.projection.csv')
with open(output_filename,'w') as f:
    fieldnames = ['x','y', 'runtime', 'representation', 'representation.target_distortion', 'representation.extra_dimensions']
    writer = csv.DictWriter(f, fieldnames = fieldnames)
    writer.writeheader()
    for (rep,td,ed) in projections:
        for i,row in enumerate(projections[(rep,td,ed)]):
            dict_row = {'x' : row[0], 'y' : row[1],
                        'runtime': runtimes[(rep,td,ed)][i],
                        'representation' : rep,
                        'representation.extra_dimensions' : ed,
                        'representation.target_distortion' : td
                        }
            writer.writerow(dict_row)
