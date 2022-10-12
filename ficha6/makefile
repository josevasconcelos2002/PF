HFLAGS=-Wall -Wextra

ficha6: ficha6.o
	ghc $(HFLAGS) $^ -o ficha6 -lm
	rm ficha6.o

ficha6.o: ficha6.hs
		ghc $(HFLAGS) -c $^
