HFLAGS=-Wall -Wextra

ficha4: ficha4.o
	ghc $(HFLAGS) $^ -o ficha4 -lm
	rm ficha4.o

ficha4.o: ficha4.hs
		ghc $(HFLAGS) -c $^