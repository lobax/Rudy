#!/bin/bash

THREADS=8
WORDS="$1"

gcc -Wall -Wextra -fopenmp -o matrixSum matrixSum-openmp.c
rm -rf graph-$WORDS.dat && touch graph-$WORDS.dat

for (( i=1; i<=$THREADS; i++ ))
do
	for j in {1..10}
	do
		TIME=`./matrixSum $WORDS $i | tail -n 1 | sed -r 's/^The execution time is ([\.0-9]+).*?$/\1/'`
		
		echo "$i;$TIME" >> graph-$WORDS.dat
	done
done

echo 'set datafile separator ";"; set yrange [0:]; set xrange [0.5:'$THREADS'.5]; set xtics 1; set terminal png truecolor enhanced font "Tahoma,10"; set ylabel "time (s)"; set xlabel "number of threads"; set output "graph-'$WORDS'.png"; set key off; plot "graph-'$WORDS'.dat" with points pointsize 2 linewidth 2' | gnuplot
