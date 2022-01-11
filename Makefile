BIN=dpll
.PHONY : all clean

$(BIN) : Main.hs CNF.hs DPLL.hs Options.hs Parser.hs Printer/Util.hs Printer/Latex.hs
	ghc -o $@ $< -dynamic --make -O2 -Wall

clean :
	rm -rf *.hi *.o $(BIN)
