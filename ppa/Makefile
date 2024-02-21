.PHONY: all

main:
	ghc src/Main.hs

# TODO:
# https://downloads.haskell.org/~ghc/6.0/docs/html/users_guide/options-output.html
# redirect compilation output

lexer:
	alex src/Lexer.x

parser:
	happy src/Parser.y

parser-debug:
	happy src/Parser.y -i