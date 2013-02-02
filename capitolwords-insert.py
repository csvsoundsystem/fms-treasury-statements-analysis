#!/usr/bin/env python2

import os, sys
import json
import dumptruck

dt = dumptruck.DumpTruck(dbname = '/tmp/capitolwords.db')

phrase = sys.argv[1]
try:
    filename = os.path.join('capitolwords', phrase + '.json')
except:
    print('USAGE: %s [phrase]' % sys.argv[0])
    exit(1)

print('Loading ' + filename)
f = open(filename)
d = json.load(f)
f.close()

dt.create_table(d['results'][0], phrase)
dt.create_index(['day'], phrase, unique = True)
dt.insert(d['results'], phrase)
