debug:
	msbuild /p:Configuration=Debug

release:
	msbuild /p:Configuration=Release

clean:
	msbuild /t:Clean /p:Configuration=Debug
	msbuild /t:Clean /p:Configuration=Release

paket:
	mono .paket/paket.exe install

restore:
	mono .paket/paket.exe restore

update:
	mono .paket/paket.exe update

all: restore debug

plots:
	gnuplot plot.gnuplot

test:
	mono --debug QuadRope.Test/bin/Debug/QuadRope.Test.exe
