all: DeltaHedging.exe

FSC = /home/${USER}/lib/fsharp/bin/fsc.exe
DLLS = -r:xunit.dll -r:fstest.dll

%.exe: DeltaHedging.fs
	mono ${FSC} --resident ${DLLS} -o $@ $<

test: DeltaHedging.fs test.fs
	mono ${FSC} --resident ${DLLS} -o test.exe $<
	mono ./test.exe
#	mono ./xunit.console.exe ./test.exe

clean:
	rm -f deltahedging.exe test.exe
