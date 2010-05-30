all: Final.dll
FSC = /home/${USER}/lib/fsharp/bin/fsc.exe
#OPTIONS= --resident

Final.dll: Final.fs
	mono ${FSC} ${OPTIONS} ${DLLS} -a $^

test: test.fs Final.dll
	mono ${FSC} ${OPTIONS} ${DLLS} -r:Final.dll $<
	mono ./test.exe

clean:
	rm -f *.exe *.dll
