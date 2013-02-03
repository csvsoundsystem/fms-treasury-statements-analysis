#!/bin/sh

for table in $(sqlite3 for-michael.db .tables); do
    sqlite3 -header -csv for-michael.db "select * from [$table];" |
    sed s/^/,/ | sed "2,$ s/^/$table/"
done
