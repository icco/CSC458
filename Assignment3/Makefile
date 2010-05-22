all: Ass3

FSC = /home/${USER}/lib/fsharp/bin/fsc.exe

Ass3: Ass3.fs
	mono ${FSC} --resident $<
	mono $@.exe

clean:
	rm *.exe

