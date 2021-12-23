.PHONY : all clean

DPLL : DPLL.hs
	ghc -o $@ $< -dynamic --make -O2 -Wall

clean :
	rm -rf *.hi *.o DPLL
