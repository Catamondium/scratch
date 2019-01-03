#!/usr/bin/env gnuplot
set key autotitle columnhead
set grid

set xlabel "X"
set xrange [-25:+25]

set ylabel "Y" rotate by 0
set yrange [-35:830]

plot 'linear.tsv' title "Linear" with lines, \
'quadratic.tsv' title "Quadratic" with lines
