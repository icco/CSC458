all: deltahedging.exe

FSC = /home/nat/lib/fsharp/bin/fsc.exe
DLLS = -r:xunit.dll -r:fstest.dll

%.exe: DeltaHedging.fs
	mono ${FSC} --resident ${DLLS} -o $@ $<

test:
	mono ./xunit.console.exe ./deltahendging.exe

clean:
	rm -f *.exe *.dll
