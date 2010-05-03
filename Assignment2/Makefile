all: DeltaHedging.exe

FSC = /home/${USER}/lib/fsharp/bin/fsc.exe
DLLS = -r:xunit.dll -r:fstest.dll

%.exe: DeltaHedging.fs
	mono ${FSC} --resident ${DLLS} -o $@ $<

test: DeltaHedging.fs test.fs
	mono ${FSC} --resident ${DLLS} $^
	mono ./test.exe

clean:
	rm -f deltahedging.exe test.exe DeltaHedging.exe
