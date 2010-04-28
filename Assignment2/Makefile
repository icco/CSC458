all: deltahedging.exe

SOURCES = DeltaHedging.fs
FSC = /home/nat/lib/fsharp/bin/fsc.exe
DLLS = -r:xunit.dll -r:fstest.dll

%.exe: ${SOURCES}
	mono ${FSC} --resident ${DLLS} -o $@ $<

test:
	mono ./xunit.console.exe ./deltahendging.exe
