.PHONY: all

all : main

clean:
	rm src/*.hi
	rm src/*.o
	rm src/Main

test:
	cd src && ghc Test*.hs

main: lexer-parser
	cd src && ghc -XImportQualifiedPost Main.hs

# TODO:
# https://downloads.haskell.org/~ghc/6.0/docs/html/users_guide/options-output.html
# redirect compilation output

lexer-parser:
	alex src/Lexer.x && happy src/Parser.y

parser-debug:
	happy src/Parser.y -i
