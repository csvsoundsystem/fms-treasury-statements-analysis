#!/bin/sh

phrase=$(echo "$1"|tr \  \+)

if [ "$phrase" = '' ]; then
  echo "Download counts of phrase usage over time."
  echo "USAGE: $0 [phrase]"
  exit 1
fi

curl --create-dirs -o "capitolwords/${phrase}.json" "http://capitolwords.org/api/1/dates.json?apikey=1bcff5b8d96e40d1a0b47665b759df9f&phrase=$phrase"
