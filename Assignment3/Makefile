all: nr

FSC = /home/${USER}/lib/fsharp/bin/fsc.exe

nr: nr.fs
	mono ${FSC} --resident $<
	mono $@.exe
