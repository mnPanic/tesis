# README

## Parser y Lexer

[Documentación de happy](https://haskell-happy.readthedocs.io/en/latest/index.html)

[Documentación de alex](https://haskell-alex.readthedocs.io/en/latest/)

## Pretty print para debugging

https://github.com/cdepillabout/pretty-simple?tab=readme-ov-file#readme
https://cdepillabout.github.io/pretty-simple/

## Cabal

Correr con args

cabal run ppa -- debug-simple.txt out

cabal run ppa -- debug-simple.txt out +RTS -p

Instalar

cabal install

### Errores comunes

Si pincha por profiling libraries, ej.

```
src/Main.hs:20:1: error:
    Could not find module ‘Text.Pretty.Simple’
    Perhaps you haven't installed the profiling libraries for package ‘pretty-simple-4.1.2.0’?
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
20 | import Text.Pretty.Simple (
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^...
```

hacer

```
cabal install --lib --enable-profiling pretty-simple --overwrite-policy=always
```

## GHCI

Para correr los tests individualmente e incluir `src/`, usar `-i`

```bash
ghci tests/TestCertifier.hs -isrc
```