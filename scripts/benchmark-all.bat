@echo off

:: This should be less complicated and convoluted.
set datetime=%date:~6,4%-%date:~3,2%-%date:~0,2%
::--%time:~0,2%-%time:~3,2%-%time:~6,2%
set logdir=benchmarks\%datetime%
mkdir %logdir%

echo Started %time% %date%.
@echo on

:: Thread scaling benchmarks for single functions.
call scripts\benchmark -m all    -s 1000 -t  1  > "%logdir%\all-s1000.out"
call scripts\benchmark -m all    -s 1000 -t  2 >> "%logdir%\all-s1000.out"
call scripts\benchmark -m all    -s 1000 -t  4 >> "%logdir%\all-s1000.out"
call scripts\benchmark -m all    -s 1000 -t  8 >> "%logdir%\all-s1000.out"
call scripts\benchmark -m all    -s 1000 -t 16 >> "%logdir%\all-s1000.out"
call scripts\benchmark -m all    -s 1000 -t 32 >> "%logdir%\all-s1000.out"
call scripts\benchmark -m all    -s 1000 -t 48 >> "%logdir%\all-s1000.out"


:: Benchmarks for indexing operations.
call scripts\benchmark -m index  -s   10 -t  1  > "%logdir%\index.out"
call scripts\benchmark -m index  -s  100 -t  1 >> "%logdir%\index.out"
call scripts\benchmark -m index  -s 1000 -t  1 >> "%logdir%\index.out"


:: Thread scaling benchmarks for mmult.
call scripts\benchmark -m mmult  -s  400 -t  1  > "%logdir%\mmult-s400.out"
call scripts\benchmark -m mmult  -s  400 -t  2 >> "%logdir%\mmult-s400.out"
call scripts\benchmark -m mmult  -s  400 -t  4 >> "%logdir%\mmult-s400.out"
call scripts\benchmark -m mmult  -s  400 -t  8 >> "%logdir%\mmult-s400.out"
call scripts\benchmark -m mmult  -s  400 -t 16 >> "%logdir%\mmult-s400.out"
call scripts\benchmark -m mmult  -s  400 -t 32 >> "%logdir%\mmult-s400.out"
call scripts\benchmark -m mmult  -s  400 -t 48 >> "%logdir%\mmult-s400.out"


:: Thread scaling benchmarks for vdc.
call scripts\benchmark -m vdc    -s   20 -t  1  > "%logdir%\vdc-s20.out"
call scripts\benchmark -m vdc    -s   20 -t  2 >> "%logdir%\vdc-s20.out"
call scripts\benchmark -m vdc    -s   20 -t  4 >> "%logdir%\vdc-s20.out"
call scripts\benchmark -m vdc    -s   20 -t  8 >> "%logdir%\vdc-s20.out"
call scripts\benchmark -m vdc    -s   20 -t 16 >> "%logdir%\vdc-s20.out"
call scripts\benchmark -m vdc    -s   20 -t 32 >> "%logdir%\vdc-s20.out"
call scripts\benchmark -m vdc    -s   20 -t 48 >> "%logdir%\vdc-s20.out"


:: Size scaling benchmarks for fib.
call scripts\benchmark -m fibs   -s  100 -t  1  > "%logdir%\fibs.out"
call scripts\benchmark -m fibs   -s  200 -t  1 >> "%logdir%\fibs.out"
call scripts\benchmark -m fibs   -s  400 -t  1 >> "%logdir%\fibs.out"
call scripts\benchmark -m fibs   -s  800 -t  1 >> "%logdir%\fibs.out"
call scripts\benchmark -m fibs   -s 1600 -t  1 >> "%logdir%\fibs.out"


:: Size scaling benchmarks for sieve.
call scripts\benchmark -m sieve  -s  100 -t  1  > "%logdir%\sieve.out"
call scripts\benchmark -m sieve  -s  200 -t  1 >> "%logdir%\sieve.out"
call scripts\benchmark -m sieve  -s  400 -t  1 >> "%logdir%\sieve.out"
call scripts\benchmark -m sieve  -s  800 -t  1 >> "%logdir%\sieve.out"
call scripts\benchmark -m sieve  -s 1600 -t  1 >> "%logdir%\sieve.out"


:: Thread scaling benchmarks for Smith-Waterman.
call scripts\benchmark -m align  -s 1000 -t  1  > "logs\benchmark-align-t16-%datetime%.out"
call scripts\benchmark -m align  -s 1000 -t  2 >> "logs\benchmark-align-t16-%datetime%.out"
call scripts\benchmark -m align  -s 1000 -t  4 >> "logs\benchmark-align-t16-%datetime%.out"
call scripts\benchmark -m align  -s 1000 -t  8 >> "logs\benchmark-align-t16-%datetime%.out"
call scripts\benchmark -m align  -s 1000 -t 16 >> "logs\benchmark-align-t16-%datetime%.out"

@echo off
echo Finished %time% %date%.
