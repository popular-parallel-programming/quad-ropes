:: Scaling benchmarks for single functions
call benchmark -s 100       >  logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  2 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  3 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  4 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  5 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  6 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  7 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  8 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t  9 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 10 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 11 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 12 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 13 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 14 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 15 >> logs\benchmark-s100-t16.txt
call benchmark -s 100 -t 16 >> logs\benchmark-s100-t16.txt

:: Scaling benchmarks for primes
call benchmark -m primes -s 100 -t 16 > logs\benchmark-primes-s100-t16.txt

:: Scaling and size scaling benchmarks for mmult
call benchmark -m mmult -s 100 -t 16 > logs\benchmark-mmult-s100-t16.txt
call benchmark -m mmult -s 200 -t 16 > logs\benchmark-mmult-s200-t16.txt

:: Size scaling benchmarks for vdc
call benchmark -m vdc -s  5 -t 1 > logs\benchmark-vdc-s05-t01.txt
call benchmark -m vdc -s 10 -t 1 > logs\benchmark-vdc-s10-t01.txt
call benchmark -m vdc -s 15 -t 1 > logs\benchmark-vdc-s15-t01.txt
call benchmark -m vdc -s 20 -t 1 > logs\benchmark-vdc-s20-t01.txt

:: Size scaling benchmarks for fibseq
call benchmark -m fibseq -s  20 -t 1 logs\benchmark-fibseq-s020-t01.txt
call benchmark -m fibseq -s  40 -t 1 logs\benchmark-fibseq-s040-t01.txt
call benchmark -m fibseq -s  80 -t 1 logs\benchmark-fibseq-s080-t01.txt
call benchmark -m fibseq -s 100 -t 1 logs\benchmark-fibseq-s100-t01.txt
