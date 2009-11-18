#!/bin/ksh

\rm -f *.pal
for txt in colormaps_*.txt ; do
  tmp=${txt#colormaps_}  ; pal=${tmp%.txt}
  cp head.PAL $pal.pal
  readcmap2 $txt >> $pal.pal
done

for cmap in *.ncmap ; do
   pal=${cmap%.ncmap}
  cp head.PAL $pal.pal
  readcmap2 $cmap >> $pal.pal
done
