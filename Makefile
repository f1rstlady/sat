BIN=dpll
.PHONY : all clean

$(BIN) : Main.hs CNF.hs DPLL.hs Printer/Util.hs
	ghc -o $@ $< -dynamic --make -O2 -Wall

clean :
	rm -rf *.hi *.o $(BIN)
