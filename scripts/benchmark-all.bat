@echo off

:: This should be less complicated and convoluted.
set datetime=%date:~6,4%-%date:~3,2%-%date:~0,2%--%time:~0,2%-%time:~3,2%-%time:~6,2%

@echo on

:: Thread scaling benchmarks for single functions.
call scripts\benchmark -m all    -s 1000 -t  1  > logs\benchmark-all-s1000-t32-%datetime%.txt
call scripts\benchmark -m all    -s 1000 -t  2 >> logs\benchmark-all-s1000-t32-%datetime%.txt
call scripts\benchmark -m all    -s 1000 -t  4 >> logs\benchmark-all-s1000-t32-%datetime%.txt
call scripts\benchmark -m all    -s 1000 -t  8 >> logs\benchmark-all-s1000-t32-%datetime%.txt
call scripts\benchmark -m all    -s 1000 -t 16 >> logs\benchmark-all-s1000-t32-%datetime%.txt
call scripts\benchmark -m all    -s 1000 -t 32 >> logs\benchmark-all-s1000-t32-%datetime%.txt


:: Thread scaling benchmarks for primes.
call scripts\benchmark -m primes -s  400 -t  1  > logs\benchmark-primes-s400-t32-%datetime%.txt
call scripts\benchmark -m primes -s  400 -t  2 >> logs\benchmark-primes-s400-t32-%datetime%.txt
call scripts\benchmark -m primes -s  400 -t  4 >> logs\benchmark-primes-s400-t32-%datetime%.txt
call scripts\benchmark -m primes -s  400 -t  8 >> logs\benchmark-primes-s400-t32-%datetime%.txt
call scripts\benchmark -m primes -s  400 -t 16 >> logs\benchmark-primes-s400-t32-%datetime%.txt
call scripts\benchmark -m primes -s  400 -t 32 >> logs\benchmark-primes-s400-t32-%datetime%.txt


:: Thread scaling benchmarks for mmult.
call scripts\benchmark -m mmult  -s  200 -t  1  > logs\benchmark-mmult-s200-t16-%datetime%.txt
call scripts\benchmark -m mmult  -s  200 -t  2 >> logs\benchmark-mmult-s200-t16-%datetime%.txt
call scripts\benchmark -m mmult  -s  200 -t  4 >> logs\benchmark-mmult-s200-t16-%datetime%.txt
call scripts\benchmark -m mmult  -s  200 -t  8 >> logs\benchmark-mmult-s200-t16-%datetime%.txt
call scripts\benchmark -m mmult  -s  200 -t 16 >> logs\benchmark-mmult-s200-t16-%datetime%.txt
call scripts\benchmark -m mmult  -s  200 -t 32 >> logs\benchmark-mmult-s200-t16-%datetime%.txt


:: Size scaling benchmarks for vdc.
call scripts\benchmark -m vdc    -s    5 -t  1  > logs\benchmark-vdc-t01-%datetime%.txt
call scripts\benchmark -m vdc    -s   10 -t  1 >> logs\benchmark-vdc-t01-%datetime%.txt
call scripts\benchmark -m vdc    -s   15 -t  1 >> logs\benchmark-vdc-t01-%datetime%.txt
call scripts\benchmark -m vdc    -s   20 -t  1 >> logs\benchmark-vdc-t01-%datetime%.txt


:: Size scaling benchmarks for fib.
call scripts\benchmark -m fibs  -s  100 -t  1  > logs\benchmark-fibs-t01-%datetime%.txt
call scripts\benchmark -m fibs  -s  200 -t  1 >> logs\benchmark-fibs-t01-%datetime%.txt
call scripts\benchmark -m fibs  -s  400 -t  1 >> logs\benchmark-fibs-t01-%datetime%.txt
call scripts\benchmark -m fibs  -s  800 -t  1 >> logs\benchmark-fibs-t01-%datetime%.txt
call scripts\benchmark -m fibs  -s 1600 -t  1 >> logs\benchmark-fibs-t01-%datetime%.txt


:: Size scaling benchmarks for sieve.
call scripts\benchmark -m sieve  -s  100 -t  1  > logs\benchmark-sieve-t01-%datetime%.txt
call scripts\benchmark -m sieve  -s  200 -t  1 >> logs\benchmark-sieve-t01-%datetime%.txt
call scripts\benchmark -m sieve  -s  400 -t  1 >> logs\benchmark-sieve-t01-%datetime%.txt
call scripts\benchmark -m sieve  -s  800 -t  1 >> logs\benchmark-sieve-t01-%datetime%.txt
call scripts\benchmark -m sieve  -s 1600 -t  1 >> logs\benchmark-sieve-t01-%datetime%.txt


:: Size scaling benchmarks for Smith-Waterman.
call scripts\benchmark -m align  -s  100 -t  1  > logs\benchmark-align-t01-%datetime%.txt
call scripts\benchmark -m align  -s  200 -t  1 >> logs\benchmark-align-t01-%datetime%.txt
call scripts\benchmark -m align  -s  400 -t  1 >> logs\benchmark-align-t01-%datetime%.txt
call scripts\benchmark -m align  -s  800 -t  1 >> logs\benchmark-align-t01-%datetime%.txt
call scripts\benchmark -m align  -s 1600 -t  1 >> logs\benchmark-align-t01-%datetime%.txt


:: Size scaling benchmarks for indexing operations.
call scripts\benchmark -m index  -s  100 -t  1  > logs\benchmark-index-t01-%datetime%.txt
call scripts\benchmark -m index  -s  200 -t  1 >> logs\benchmark-index-t01-%datetime%.txt
call scripts\benchmark -m index  -s  400 -t  1 >> logs\benchmark-index-t01-%datetime%.txt
call scripts\benchmark -m index  -s  800 -t  1 >> logs\benchmark-index-t01-%datetime%.txt
call scripts\benchmark -m index  -s 1600 -t  1 >> logs\benchmark-index-t01-%datetime%.txt
