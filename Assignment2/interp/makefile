all : sample.exe

SAMPLESOURCES = AST.fs Values.fs SampleInterp.fs
FSC = ~/FSharp/bin/fsc.exe
DLLS = -r:xunit.dll -r:fstest.dll

sample.exe : ${SAMPLESOURCES}
	mono ${FSC} --resident ${DLLS} -o sample.exe ${SAMPLESOURCES}

sampleruntests :
	mono ./xunit.console.exe ./sample.exe
