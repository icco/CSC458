all: DeltaHedging.dll

FSC = /home/${USER}/lib/fsharp/bin/fsc.exe
DLLS = -r:xunit.dll -r:fstest.dll

%.dll: DeltaHedging.fs
	mono ${FSC} --resident ${DLLS} -a $^

test: test.fs DeltaHedging.dll
	mono ${FSC} --resident ${DLLS} -r:DeltaHedging.dll $<
	mono ./test.exe

clean:
	rm -f deltahedging.exe test.exe DeltaHedging.exe DeltaHedging.dll
