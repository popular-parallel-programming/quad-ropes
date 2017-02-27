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
	gnuplot plot.gnuplot

test:
	mono --debug QuadRope.Test/bin/Debug/QuadRope.Test.exe
