.PHONY: all

main:
	ghc Main.hs

lexer:
	alex Lexer.x

parser:
	happy Parser.y

parser-debug:
	happy Parser.y -i