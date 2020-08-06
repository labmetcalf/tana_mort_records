#!/bin/bash
mkdir -p $1
cd $1
isol=$(echo "$2" | tr '[:upper:]' '[:lower:]')
yr="$3"
suff="_f_{}_$yr.tif"
urlf="ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/$yr/$2/$isol$suff"
urlm="ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020/$yr/$2/$isol$suff"
echo "$urlf"
echo "$urlm"

parallel --jobs 10 curl -O -s $urlf ::: {0..80}
parallel --jobs 10 curl -O -s $urlm ::: {0..80}
