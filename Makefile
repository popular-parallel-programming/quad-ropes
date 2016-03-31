build:
	xbuild QuadRope.sln

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

all: paket build

clean:
	xbuild /t:Clean
