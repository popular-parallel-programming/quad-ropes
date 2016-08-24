:: Scaling benchmarks for single functions
call benchmark       > logs\benchmark-s1000-t01.txt
call benchmark -t 2  > logs\benchmark-s1000-t02.txt
call benchmark -t 3  > logs\benchmark-s1000-t03.txt
call benchmark -t 4  > logs\benchmark-s1000-t04.txt
call benchmark -t 5  > logs\benchmark-s1000-t05.txt
call benchmark -t 6  > logs\benchmark-s1000-t06.txt
call benchmark -t 7  > logs\benchmark-s1000-t07.txt
call benchmark -t 8  > logs\benchmark-s1000-t08.txt
call benchmark -t 9  > logs\benchmark-s1000-t09.txt
call benchmark -t 10 > logs\benchmark-s1000-t10.txt
call benchmark -t 11 > logs\benchmark-s1000-t11.txt
call benchmark -t 12 > logs\benchmark-s1000-t12.txt
call benchmark -t 13 > logs\benchmark-s1000-t13.txt
call benchmark -t 14 > logs\benchmark-s1000-t14.txt
call benchmark -t 15 > logs\benchmark-s1000-t15.txt
call benchmark -t 16 > logs\benchmark-s1000-t16.txt

:: Scaling benchmarks for primes
call benchmark -m primes -s 100 -t 16 > logs\benchmark-primes-s100-t16.txt

:: Scaling benchmarks for mmult
call benchmark -m mmult -s 200 -t 16 > logs\benchmark-mmult-s200-t01.txt
