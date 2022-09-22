HFLAGS=-Wall -Wextra

ficha5: ficha5.o
	ghc $(HFLAGS) $^ -o ficha5 -lm
	rm ficha5.o

ficha5.o: ficha5.hs
		ghc $(HFLAGS) -c $^