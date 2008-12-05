all:
	ghc -O -i. -i../lib Main.hs --make -o test-permute
	./test-permute

hpc:
	ghc -fforce-recomp -i. -i../lib -fhpc --make Main.hs -o test-permute
	rm -f test-permute.tix
	./test-permute
	hpc markup test-permute

clean:
	find ../lib . -name '*.hi' | xargs rm -f
	find ../lib . -name '*.o'  | xargs rm -f
	find . -name '*.html' | xargs rm -f
	rm -f test-permute test-permute.tix
	