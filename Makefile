debug:
	xbuild /p:Configuration=Debug /p:TargetFrameworkVersion="v4.5"

release:
	xbuild /p:Configuration=Release /p:TargetFrameworkVersion="v4.5"

clean:
	xbuild /t:Clean /p:Configuration=Debug
	xbuild /t:Clean /p:Configuration=Release

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

all: paket debug

plots:
	gnuplot gnuplot/primes.gnuplot
	gnuplot gnuplot/mmult.gnuplot
	gnuplot gnuplot/map.gnuplot
	gnuplot gnuplot/reduce.gnuplot
