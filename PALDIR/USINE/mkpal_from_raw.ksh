#!/bin/ksh


for f in blu_red9.raw ; do
  pal=${f%.raw}.pal
  cat head.PAL > $pal

  cat $f | awk '{ printf "%d %6.4f %6.4f %6.4f \n ", NR+19,$1/255.,$2/255.,$3/255. }' >> $pal
done
