#!/bin/bash

datetime=`date "+%Y%m%d_%H%M%S"`
filename="./result${datetime}-${1}vs${2}.sam"
./manager/gameManager $1 $2 >> ${filename}
