as: as.o
	clang as.o write.c

as.o: as.s
	as -arch arm64 -o as.o as.s

as.s: Main digit.b
	./Main digit.b

Main: *.hs
	ghc Main.hs
