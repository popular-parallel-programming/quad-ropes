set term postscript

# Labels are the same for all plots.
set xlabel "Number of hardware-threads"
set ylabel "Time elapsed in ns"

# Plot reduce benchmarks
set output "benchmark-reduce-s1000-t16.ps"
set title "Performance of \texttt{reduce} over double arrays."
plot "logs/benchmark-s1000-t16.txt" every :::4::4   using 2:3:5 title "2D array"     with errorlines,\
     "logs/benchmark-s1000-t16.txt" every :::12::12 using 2:3:5 title "Quad rope $s_{\max} = 16$" with errorlines,\
     "logs/benchmark-s1000-t16.txt" every :::20::20 using 2:3:5 title "Quad rope $s_{\max} = 32$" with errorlines

# Plot map benchmarks
set output "benchmark-map-s1000-t16.ps"
set title "Performance of \texttt{map} over double arrays."
plot "logs/benchmark-s1000-t16.txt" every :::3::3   using 2:3:5 title "2D array"     with errorlines,\
     "logs/benchmark-s1000-t16.txt" every :::11::11 using 2:3:5 title "Quad rope $s_{\max} = 16$" with errorlines,\
     "logs/benchmark-s1000-t16.txt" every :::19::19 using 2:3:5 title "Quad rope $s_{\max} = 32$" with errorlines

# Plot mmult benchmarks
set output "benchmark-mmult-s100-t04.ps"
set title "Performance of \texttt{mmult} on 4-core i7, size $100 \times 100$."
plot "logs/benchmark-mmult-s100-t04.txt" every 2::0 using 2:3:5 title "2D array"  with errorlines,\
     "logs/benchmark-mmult-s100-t04.txt" every 2::1 using 2:3:5 title "Quad rope $s_{\max} = 32$" with errorlines

set output "benchmark-mmult-s200-t04.ps"
set title "Performance of \texttt{mmult} on 4-core i7, size $200 \times 200$."
plot "logs/benchmark-mmult-s200-t04.txt" every 2::0 using 2:3:5 title "2D array"  with errorlines,\
     "logs/benchmark-mmult-s200-t04.txt" every 2::1 using 2:3:5 title "Quad rope $s_{\max} = 32$" with errorlines

set output "benchmark-mmult-s200-t16.ps"
set title "Performance of \texttt{mmult} on 16-core i7, size $200 \times 200$."
plot "logs/benchmark-mmult-s200-t16.txt" every 2::0 using 2:3:5 title "2D array"  with errorlines,\
     "logs/benchmark-mmult-s200-t16.txt" every 2::1 using 2:3:5 title "Quad rope $s_{\max} = 32$" with errorlines

# Plot primes benchmarks
set output "benchmark-primes-s100-t16.ps"
set title "Performance of \texttt{primes} on 16-core i7, size $100 \times 100$."
plot "logs/benchmark-primes-s100-t16.txt" every 2::0 using 2:3:5 title "Array2D"  with errorlines,\
     "logs/benchmark-primes-s100-t16.txt" every 2::1 using 2:3:5 title "QuadRope $s_{\max} = 32$" with errorlines
