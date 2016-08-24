set term postscript
set xlabel "Number of hardware-threads"
set ylabel "Time elapsed in ns"
set output "benchmark-map-s1000-t16.ps"
plot "logs/benchmark-s1000.txt" every :::3::3   using 2:3:5 title "Array2D.map"  with errorlines,\
     "logs/benchmark-s1000.txt" every :::11::11 using 2:3:5 title "QuadRope.map" with errorlines
