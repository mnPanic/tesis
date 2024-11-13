.PHONY: all

all : main

clean:
	rm src/*.hi
	rm src/*.o
	rm src/Main
	rm -rf dist-newstyle
	rm src/*.dyn_o
	rm src/*.dyn_hi

test:
	cd src && ghc Test*.hs

# https://pusher.com/blog/top-tips-and-tools-for-optimising-haskell/#introduction
main: lexer-parser
	cd src && ghc -XImportQualifiedPost -funfolding-use-threshold=16 -O2 -optc-O3 Main.hs

# TODO:
# https://downloads.haskell.org/~ghc/6.0/docs/html/users_guide/options-output.html
# redirect compilation output

lexer-parser:
	alex src/PPA/Lexer.x && happy src/PPA/Parser.y

parser-debug:
	happy src/Parser.y -i
