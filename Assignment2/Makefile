all: deltahedging.exe

FSC = /home/nat/lib/fsharp/bin/fsc.exe
#FSC = ~jcswenso/fsc
DLLS = -r:xunit.dll -r:fstest.dll

%.exe: DeltaHedging.fs
	mono ${FSC} --resident ${DLLS} -o $@ $<

test: deltahedging.exe
	mono ./xunit.console.exe ./deltahendging.exe

clean:
#	rm -f *.exe *.dll
