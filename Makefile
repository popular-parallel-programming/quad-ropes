debug:
	xbuild /p:Condiguration=Debug

release:
	xbuild /p:Configuration=Release

clean:
	xbuild /t:Clean /p:Configuration=Debug
	xbuild /t:Clean /p:Configuration=Release

paket:
	mono .paket/paket.exe install

update:
	mono .paket/paket.exe update

all: paket debug
