set term postscript
set xlabel "Number of hardware-threads"
set ylabel "Time elapsed in ns"
set output "benchmark-mmult-s100-t16.ps"
plot "logs/benchmark-mmult-s100-t16.txt" every 2::0 using 2:3:5 title "Array2D"  with errorlines,\
     "logs/benchmark-mmult-s100-t16.txt" every 2::1 using 2:3:5 title "QuadRope" with errorlines
