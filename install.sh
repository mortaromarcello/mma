#!/bin/bash
dest="/usr/local/share/mma"
exe="/usr/local/bin/mma"
mkdir -p $dest
cp -v mma.py $exe
for dir in lib includes MMA docs; do
  cp -r $dir $dest
done
$exe -G
chmod a+w $dest/lib/stdlib/.mmaDB
