all:
	ghc -O -i. -i.. `gsl-config --libs-without-cblas` Unit.hs --make
	./Unit

clean:
	rm -f *~ Unit *.hi *.o \
		../Data/Permutation.hi ../Data/Permutation.o \
		../System/Random/Permutation.hi ../System/Random/Permutation.o
