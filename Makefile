build:
	xbuild

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

test:
	fsharpi --mlcompatibility --exec Trees/QuadRope.Test.fsx

all: paket build
