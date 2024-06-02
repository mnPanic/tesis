# PPA Syntax

PPA (Pani's Proof Assistant) es un lenguaje que permite escribir demostraciones
en LPO. Al ejecutarse, certifica las demostraciones generando demostraciones en
deducción natural (DN).

## Programas

Los programas están compuestos por **declaraciones**, que pueden ser

- **Axiomas**

  `axiom <name> : <form>`

- **Teoremas**

  ```text
  theorem <name> : <form>
  proof
    <steps>
  end
  ```

## Fórmulas y términos

Las fórmulas y términos de primer orden son

**Identificadores**:

- Variables (`<var>`): Comienzan por `_` o mayúsculas y siguen con alfanuméricos.
  Opcionalmente terminan en `'`

  `(\_|[A-Z])[a-zA-Z0-9\_\-]*(\')*`

- Funciones y predicados (`<id>`): Todo lo que no son variables, con más símbolos

  `[a-zA-Z0-9\_\-\?!#\$\%&\*\+\<\>\=\?\@\^]+(\')*`

- Nombres (`<name>`): Pueden ser o bien identificadores, o si están entre
  comillas pueden tener cualquier símbolo dentro (incluso espacios).

**Fórmulas**:

- **Predicados**: `<id>(<term>, ..., <term>)`

  > Los argumentos son opcionales

- **Conectivos binarios**: and, or, implicación

  ```text
  <form> | <form>
  <form> & <form>
  <form> -> <form>
  ```

- **Negación**: `~ <form>`
- **Cuantificadores**: `exists <var> . <form>`, `forall <var> . <form>`
- **Paréntesis**: `( <form> )`
- `true`, `false`

Términos:

- **Funciones**: `<id>(<term>, ..., <term>)`
- **Variables**: `<var>`

## Comentarios

Se pueden dejar comentarios de una sola línea (`//`) o multilínea (`/* ... */`)

## Demostraciones

Las demostraciones están compuestas por *pasos*, que deben probar la *tesis*.
Los pasos corresponden a *comandos*.

### Comandos

Para demostrar una tesis, hay que **reducirla** mediante comandos hasta agotarla.

#### By

El principal mecanismo de demostración es el **by**, que afirma que un hecho es
consecuencia de una lista de hipótesis. Esto permite eliminar universales e implicaciones.

Puede usarse de dos formas principales

- `thus <form> by <justification>`: Si form es *parte* de la tesis (ver
  [descarga de conjunciones](#descarga-de-conjunciones)), y es
  consecuencia lógica de las justificaciones, lo demuestra automáticamente y lo
  descarga de la tesis.

  Por ejemplo, para eliminación de implicaciones

  ```text
  axiom ax_1 : a -> b
  axiom ax_2 : b -> c
  theorem t1 : a -> c 
  proof
      suppose a : a
      // La tesis ahora es c
      thus c by a, ax_1, ax_2
  end
  ```

  Y para eliminación de cuantificadores universales,

  ```text
  axiom ax_1 : forall X . f(X)
  
  theorem t1: f(n)
  proof
    thus f(n) by ax_1
  end
  ```

- `have <form> by <justification>`: Es como `thus` pero para afirmaciones
  *auxiliares* que no son parte de la tesis. No reduce la tesis.

  Por ejemplo,

  ```text
  theorem "ejemplo" : (a -> b -> c) -> (a -> b) -> a -> c
  proof
      suppose "P": a -> b -> c
      suppose "Q": a -> b
      suppose "R": a
      have "S": b by "Q", "R"
      thus c   by "P", "R", "S"
  end
  ```

Ambas tienen su contraparte con azúcar sintáctico que agrega automáticamente la
hipótesis anterior a la justificación, a la que también se puede referir con `-`.

| **Comando** | **Azúcar** | **Reduce la tesis** |
| ----------- | ---------- | ------------------- |
| `thus`      | `hence`    | Si                  |
| `have`      | `then`     | No                  |

El by es opcional en ambos. En caso de no especificarlo, debe ser una tautología.

### Comandos y reglas de inferencia

- **`suppose`**: Corresponde a la introducción de la implicación (`=>-I`)

  `suppose <hyp name> : <form>`

  Si la tesis es `A -> B`, asume `A` y la tesis se convierte en `B`.

  Viendo a la negación como implicación, `~A = A -> bot`, se puede usar
  `suppose` para razonar por el absurdo.

  ```text
  // proof de ~A
  suppose - : A
  // ... proof de false
  ```

- **`cases`**: Corresponde a la eliminación de la disyunción (`v-E`)

  Permite razonar por casos. Para cada uno de ellos se debe demostrar la tesis
  en su totalidad por separado.
  
  ```text
  theorem "ejemplo cases": (a & b) | (c & a) -> a
  proof
      suppose h : (a & b) | (c & a)
      cases by h
          case a&b
              hence a
          case h:a&c // no tiene que ser igual
              thus a by h
      end
  end
  ```

  El `by` es opcional, para casos en donde la disyunción es provable sin
  antecedentes (por ejemplo si tiene la forma `a | ~a` (LEM))

- **`take`** o introducción del existencial

  Para probar un existencial se usa el comando `take`. Si la tesis es `exists X.
  p(X)`, luego del comando `take X := a` la tesis se reduce a `p(a)`

- **`consider`** o eliminación del existencial

  Si se puede justificar `exists X . p`, se puede razonar sobre ese x.
  
  El comando `consider X st h : p by ...` agrega f al contexto para el resto de la demostración siempre y cuando `X` no aparezca libre en la tesis o el contexto hasta el momento.

  El `by` debe justificar `exists X . p`.

- **`let`** o introducción de universal
  
  Para probar un cuantificador universal `forall X. p(X)`, luego del comando
  `let X := Y` la tesis se reduce a `p(Y)` para un `Y` genérico.
  
  Puede ser el
  mismo nombre de variable, en ese caso se puede escribir directamente como `let X`.

### Otros comandos

- **`equivalently`**: Permite reducir la tesis a una fórmula equivalente

  Ejemplo:

  ```text
  theorem "ejemplo" : ¬(a | b)
  proof
    equivalently (¬a & ¬b)
    ...
  end
  ```

- **`claim`**: Permite hacer una afirmación auxiliar, junto con su demostración

  Ejemplo:

  ```text
  theorem "ejemplo" : ¬(a | b)
  proof
    claim "c" : (¬a & ¬b)
    proof
      ...
    end
    thus ¬(a | b) by "c"
  end
  ```

### Descarga de conjunciones

Si la tesis es una conjunción, se puede probar solo una parte de ella y luego la
tesis se reduce al resto. Por ejemplo,

```text
theorem "and discharge" : a -> b -> (a & b)
proof
    suppose "a" : a
    suppose "b" : b
    // La tesis es a & b
    hence b by "b"

    // La tesis es a
    thus a by "a"
end
```

Y también puede ser de forma más compleja, parcialmente y en cualquier órden

```text
axiom "a": a
axiom "b": b
axiom "c": c
axiom "d": d
axiom "e": e
theorem "and discharge" : (a & b) & ((c & d) & e)
proof
    thus a & e by "a", "e"
    thus d by "d"
    thus b & c by "b", "c"
end
```