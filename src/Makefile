as: as.o write.o
	clang as.o write.o

as.o: as.s
	as -arch arm64 -o as.o as.s

as.s: Main examples/ifelse.b
	./Main examples/ifelse.b

Main: *.hs
	ghc Main.hs

write.o: write.c
	clang -c write.c -o write.o
