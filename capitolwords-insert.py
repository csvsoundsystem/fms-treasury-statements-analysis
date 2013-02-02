#!/usr/bin/env python2

import os, sys
import dumptruck

dt = dumptruck.DumpTruck(dbname = '/tmp/capitolwords.db')

f = open(sys.argv([1]))
d = json.load(f)
f.close()

dt.insert(d['results'])
