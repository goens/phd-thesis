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
representation = sys.argv[2]
target_distortion = sys.argv[3]
extra_dimensions = sys.argv[4]

mappings = []
runtimes = []
with open(filename,'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        if representation == row['representation'] and target_distortion == row['representation.target_distortion'] and extra_dimensions == row['representation.extra_dimensions']:
            mapping = parse_mapping(row['mapping'])
            runtime = float(row['runtime'])
            mappings.append(mapping)
            runtimes.append(runtime)

mappings_matrix = np.array(mappings).reshape((len(runtimes),-1))
print(f"read mappings matrix (shape {mappings_matrix.shape})")
projection = jlt(mappings_matrix,2)
print(f"projected onto 2 dimensions (with {len(np.unique(projection))} unique entries)")
output_filename = filename.replace('.csv', '.projection.csv')
with open(output_filename,'w') as f:
    fieldnames = ['x','y', 'runtime']
    writer = csv.DictWriter(f, fieldnames = fieldnames)
    writer.writeheader()
    for i,row in enumerate(projection):
        dict_row = {'x' : row[0], 'y' : row[1], 'runtime':runtimes[i]}
        writer.writerow(dict_row)
