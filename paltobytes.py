#!/usr/bin/python2

from sys import argv, exit

if len(argv) < 3:
    print "Usage: paltobytes infile outfile"
    exit(0)

inpath = argv[1]
outpath = argv[2]

with open(inpath, 'r') as infile:
    infile.readline()
    infile.readline()

    count = int(infile.readline())
    with open(outpath, 'wb') as outfile:
        for i in range(count):
            r, g, b = map(int, infile.readline().split())
            outfile.write('%c%c%c' % (r / 4, g / 4, b / 4))
