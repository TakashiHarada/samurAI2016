#!/bin/bash -v

datetime=`date "+%Y%m%d_%H%M%S"`
echo $datetime
filename="./result${datetime}-${1}vs${2}.sam"
echo $filename
./manager/gameManager $1 $2 >> ${filename}
