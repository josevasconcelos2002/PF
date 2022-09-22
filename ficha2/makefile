HFLAGS=-Wall -Wextra

ficha2: ficha2.o
	ghc $(HFLAGS) $^ -o ficha2 -lm
	rm ficha2.o
	rm ficha2.hi

ficha2.o: ficha2.hs
		ghc $(HFLAGS) -c $^