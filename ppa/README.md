# PPA

PPA - *Pani's proof assistant*.

Para la sintaxis del lenguaje, ver [`doc/docs.md`](doc/docs.md).

## Instalación

Con `cabal`

```bash
cabal install ppa
```

Si falla por que ya está instalado y se quiere re-instalar sin cambiar la
versión,

```bash
cabal install ppa --overwrite-policy=always
```

## Uso

El ejecutable `ppa` tiene dos comandos: `check` y `extract`.

Para más información sobre cómo funciona el lenguaje y cómo escribir
demostraciones, leer el capítulo "PPA el lenguaje" de la tesis o leer el
documento de sintaxis del lenguaje: [`doc/docs.md`](doc/docs.md), que es lo mismo con menos
sofisticación.

Hay ejemplos en el directorio [`doc/examples`](doc/examples/)

### `check` - Chequeo de demostraciones

Uso:

```bash
ppa check <in> --out <path>
```

- `<in>` (obligatorio): El path al archivo que contiene el programa de PPA a
  chequear. Opcionalmente puede ser `-` en cuyo caso lo toma por stdin
- `--out <path>` (opcional): El archivo de output para el certificado, con nombre
  `<out>_raw.nk`. Puede ser también `-` en cuyo caso lo escribe por `stdout`.

Certifica y chequea el programa. Ejemplo:

```bash
$ ppa check doc/examples/extract/alumnos.ppa               
Checking... OK!
```

```bash
ppa check doc/examples/extract/alumnos.ppa --out out
Checking... OK!
Writing...
Wrote raw to out_raw.nk
```

### `extract` - Extracción de testigos

```bash
ppa extract <in>
```

Argumentos:

- `--theorem` / `-t` (obligatorio): El nombre del teorema a partir del cual
  extraer el testigo
- `--terms` / `-ts`: Lista de términos para instanciar variables cuantificadas
  universalmente.
- `--out` / `-o` (opcional): El archivo de output para el certificado. Puede ser
  un nombre como `out` o `-` para stdout.

  Genera los archivos `<out>_raw.nk` (certificado clásico) y `<out>.nj`
  (certificado traducido a intuicionista y reducido)

Ejemplos

```bash
$ ppa extract doc/examples/extract/simple-forall.ppa -t t --terms x y
Running program... OK!
Translating... OK!
Checking translated... OK!
Extracted witness: v
of formula: p(v, x, y)
```

Con output

```bash
$ ppa extract doc/examples/extract/simple-forall.ppa -t t --terms x y -o out
Running program... OK!
Translating... OK!
Writing...
Wrote raw to out_raw.nk
Wrote translated+reduced to out.nj
Checking translated... OK!
Extracted witness: v
of formula: p(v, x, y)
```

## Desarrollo

Para correr los tests individualmente, como no están en el mismo directorio hay
que incluir `src` con `-i`.

```bash
ghci tests/TestCertifier.hs -isrc
```

### Grafo de módulos

Usando https://github.com/yav/graphmod

```bash
find src/ -name '*.hs' | xargs graphmod -i src -d 15,10 | tred | dot -Tpng > modules.png
```