# Notas de Programming in Haskell de Graham Hutton

Why Functional Programming Matters

> functional programming can be viewed as a style of programming in which the
> basic method of computation is the application of functions to arguments

Recomienda leer parte I entera, y después seleccionar los tópicos de la parte II
que sean de interés. Vital ir escribiendo código mientras se hace follow-along.

## 1 - Introduction

Contenidos:

- Noción de función
- Programación funcional
- Features principales de haskell
- Tres ejemplos chiquitos

### Funciones

Mapeos de uno o más argumentos a un resultado único. Se define con una ecuación,
ej.

```haskell
double x = x + x
```

Cuando se **aplica** una función a argumentos, el resultado se obtiene
sustituyendo los argumentos en el cuerpo de la función. Esto puede producir un
resultado que no se puede simplificar más, pero usualmente el resultado es una
*expresión* que contiene otras aplicaciones a funciones que deben ser
procesadas.

Para procesar `double (double 2)` hay dos formas, primero hacer el inner y
después el outer, o primero hacer el outer y después el inner. El resultado es
el mismo pero una tiene más pasos que la otra (porque se duplica el cómputo de
`double 2` cuando se hace la de afuera primero). Esto puede hacer que algunos
cómputos no terminen, que se vé más en detalle en el chapter 15.

### Programación funcional

Es un *estilo* de programación en donde el método básico de cómputo es la
aplicación de funciones a argumentos. Un lenguaje de programación funcional no
es más que uno que *soporta* y *alienta* ese estilo.

Ejemplo: computar la suma de los números de 1 a n

En java sería

```java
int total = 0;
for (int count = 1; count <= n; count++)
  total = total + count;
```

El método de cómputo es *changing stored values*, en el sentido de que la
ejecución del programa resulta en una secuencia de asignaciones. Por ejemplo,
con n = 3

```java
total = 0;
count = 1;
total = 1;
count = 2;
total = 3;
count = 3;
total = 6;
```

en general los lenguajes con este modelo de cómputo se llaman **imperativos**,
porque los programas se construyen de instrucciones imperativas que especifican
precisamente cómo debería proceder el cómputo.

En Haskell, se haría usando dos lib functions `sum [1..n]`. Acá el método de
cómputo principal es *aplicar funciones a argumentos*: la ejecución del programa
resulta en una secuencia de aplicaciones.

Muchos lenguajes imperativos permiten programar de esta forma, pero no lo
alientan.

### Features de haskell

- Programas concisos
- Type system poderoso, con inferencia de tipos.
- Funciones genéricas
- Evaluación lazy
- Equational reasoning