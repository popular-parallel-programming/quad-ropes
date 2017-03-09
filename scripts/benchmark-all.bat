@echo off

:: This should be less complicated and convoluted.
set datetime=%date:~6,4%-%date:~3,2%-%date:~0,2%--%time:~0,2%-%time:~3,2%-%time:~6,2%

@echo on

:: Thread scaling benchmarks for single functions.
call scripts\benchmark -m all    -s 1000 -t  1  > "logs\benchmark-all-s1000-t16-%datetime%.txt"
call scripts\benchmark -m all    -s 1000 -t  2 >> "logs\benchmark-all-s1000-t16-%datetime%.txt"
call scripts\benchmark -m all    -s 1000 -t  4 >> "logs\benchmark-all-s1000-t16-%datetime%.txt"
call scripts\benchmark -m all    -s 1000 -t  8 >> "logs\benchmark-all-s1000-t16-%datetime%.txt"
call scripts\benchmark -m all    -s 1000 -t 16 >> "logs\benchmark-all-s1000-t16-%datetime%.txt"


:: Benchmarks for indexing operations.
call scripts\benchmark -m index  -s 1000 -t  1  > "logs\benchmark-index-t01-%datetime%.txt"


:: Thread scaling benchmarks for mmult.
call scripts\benchmark -m mmult  -s  100 -t  1  > "logs\benchmark-mmult-s100-t16-%datetime%.txt"
call scripts\benchmark -m mmult  -s  100 -t  2 >> "logs\benchmark-mmult-s100-t16-%datetime%.txt"
call scripts\benchmark -m mmult  -s  100 -t  4 >> "logs\benchmark-mmult-s100-t16-%datetime%.txt"
call scripts\benchmark -m mmult  -s  100 -t  8 >> "logs\benchmark-mmult-s100-t16-%datetime%.txt"
call scripts\benchmark -m mmult  -s  100 -t 16 >> "logs\benchmark-mmult-s100-t16-%datetime%.txt"


:: Thread scaling benchmarks for vdc.
call scripts\benchmark -m vdc    -s   20 -t  1  > "logs\benchmark-vdc-s20-t16-%datetime%.txt"
call scripts\benchmark -m vdc    -s   20 -t  2 >> "logs\benchmark-vdc-s20-t16-%datetime%.txt"
call scripts\benchmark -m vdc    -s   20 -t  4 >> "logs\benchmark-vdc-s20-t16-%datetime%.txt"
call scripts\benchmark -m vdc    -s   20 -t  8 >> "logs\benchmark-vdc-s20-t16-%datetime%.txt"
call scripts\benchmark -m vdc    -s   20 -t 16 >> "logs\benchmark-vdc-s20-t16-%datetime%.txt"


:: Size scaling benchmarks for fib.
call scripts\benchmark -m fibs   -s  100 -t  1  > "logs\benchmark-fibs-t01-%datetime%.txt"
call scripts\benchmark -m fibs   -s  200 -t  1 >> "logs\benchmark-fibs-t01-%datetime%.txt"
call scripts\benchmark -m fibs   -s  400 -t  1 >> "logs\benchmark-fibs-t01-%datetime%.txt"
call scripts\benchmark -m fibs   -s  800 -t  1 >> "logs\benchmark-fibs-t01-%datetime%.txt"
call scripts\benchmark -m fibs   -s 1600 -t  1 >> "logs\benchmark-fibs-t01-%datetime%.txt"


:: Size scaling benchmarks for sieve.
call scripts\benchmark -m sieve  -s  100 -t  1  > "logs\benchmark-sieve-t01-%datetime%.txt"
call scripts\benchmark -m sieve  -s  200 -t  1 >> "logs\benchmark-sieve-t01-%datetime%.txt"
call scripts\benchmark -m sieve  -s  400 -t  1 >> "logs\benchmark-sieve-t01-%datetime%.txt"
call scripts\benchmark -m sieve  -s  800 -t  1 >> "logs\benchmark-sieve-t01-%datetime%.txt"
call scripts\benchmark -m sieve  -s 1600 -t  1 >> "logs\benchmark-sieve-t01-%datetime%.txt"


:: Thread scaling benchmarks for Smith-Waterman.
call scripts\benchmark -m align  -s 1000 -t  1  > "logs\benchmark-align-t16-%datetime%.txt"
call scripts\benchmark -m align  -s 1000 -t  2 >> "logs\benchmark-align-t16-%datetime%.txt"
call scripts\benchmark -m align  -s 1000 -t  4 >> "logs\benchmark-align-t16-%datetime%.txt"
call scripts\benchmark -m align  -s 1000 -t  8 >> "logs\benchmark-align-t16-%datetime%.txt"
call scripts\benchmark -m align  -s 1000 -t 16 >> "logs\benchmark-align-t16-%datetime%.txt"
