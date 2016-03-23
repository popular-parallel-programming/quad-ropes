build:
	xbuild QuadRope/QuadRope.fsproj

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

test: build
	xbuild QuadRope.Test/QuadRope.Test.fsproj
	mono QuadRope.Test/bin/Debug/QuadRope.Test.exe

all: paket build
