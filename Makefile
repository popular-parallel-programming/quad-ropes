build:
	xbuild

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

all: paket build
