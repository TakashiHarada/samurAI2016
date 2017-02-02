#!/bin/bash

datetime=`date "+%Y%m%d_%H%M%S"`
filename="./hambda-${datetime}.tar.gz"
tar zcvf ${filename} run.sh compile.sh icon.png Makefile *.hs
