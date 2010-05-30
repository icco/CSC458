all: Final.dll

FSC = /home/${USER}/lib/fsharp/bin/fsc.exe

Final.dll: Final.fs
	mono ${FSC} --resident ${DLLS} -a $^

test: test.fs Final.dll
	mono ${FSC} --resident ${DLLS} -r:DeltaHedging.dll $<
	mono ./test.exe

clean:
	rm -f *.exe *.dll
