#!/usr/bin/env bash

raco pollen publish src pub
raco pollen render -s src/
cp src/*.html pub/
cp src/scribbles/*.html pub/scribbles
cp src/others/*.html pub/others
