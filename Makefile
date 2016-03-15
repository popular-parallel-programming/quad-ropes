build:
	xbuild

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

test: build
	mono Trees.Test/bin/Debug/Trees.Test.exe

all: paket build
