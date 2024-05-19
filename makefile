all: 
	cabal clean
	cabal build hasky
	find dist-newstyle -name hasky -type f -exec cp -n {} . \;

clean:
	rm -f hasky
	cabal clean	
