HFLAGS=-Wall -Wextra

ficha1: ficha1.o
	ghc $(HFLAGS) $^ -o ficha1 -lm
	rm ficha1.o

ficha1.o: ficha1.hs
		ghc $(HFLAGS) -c $^