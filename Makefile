a.out: src/main.hs
	ghc -O2 -o a.out src/main.hs

prof:
	ghc -prof -fprof-auto -rtsopts Main.hs
