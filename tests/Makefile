all:
	ghc -O -i. -i../lib Main.hs --make -o test-permutation
	./test-permutation

hpc:
	ghc -fforce-recomp -i. -i../lib -fhpc --make Main.hs -o test-permutation
	rm -f test-permutation.tix
	./test-permutation
	hpc markup test-permutation

clean:
	find ../lib . -name '*.hi' | xargs rm -f
	find ../lib . -name '*.o'  | xargs rm -f
	find . -name '*.html' | xargs rm -f
	rm -f test-permutation test-permutation.tix
	rm -rf .hpc
	