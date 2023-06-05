## Cyclic Shifts

https://dmoj.ca/problem/ccc20j4

Una rotacion de una cadena es cuando desplazamos todos los caracteres de una cadena 1 indice hacia delante (o hacia atras), con el correspondiente desplazamiento del primer caracter hacia la ultima posicion (o viceversa). Por ejemplo

cadena, adenac, denaca, enacad, nacade, acaden

son todas rotaciones de la palabra cadena


En este problema, la entrada consiste en dos cadenas T y s y nuestra tarea es descubrir si en la cadena T aparece alguna rotacion de la cadena s

Para solucionar este probema usando el paradigma de la programacion declarativa con Haskell, definimos la funcion shifts, que devuelve la sucesion de las rotaciones de una cadena. Definimos ademas la funcion get_substrings que nos devuelve todas las subcadenas de un tamanho determinado de una cadena. Luego, solo nos queda encontrar si hay alguna cadena que es tanto una rotacion de s como una subcadena de T y por tanto es comun en las listas producidas por shifts s y por get_substrings T length s.

La complejidad de esta solucion es cuadratica; pero dada las constraints del problema (cadenas de tamanho hasta 1000), pasa todos los casos de prueba sin inconvenientes

## Bruno and Fibonacci
https://dmoj.ca/problem/gfsspc1p4

En este problema debemos comprobar si una cadena dada cumple que tiene letras A solo en las posiciones correspondientes a indices que estan en la sucesion de Fibonacci.

Dado que la longitud de la cadena no sobrepasa 500 entonces no necesitamos mas alla de los primeros 15 valores diferentes de Fibonacci

Luego, el analizar si la cadena tiene a la A solo en posiciones con indices de Fibonacci, se puede hacer de manera recursiva, analizando indice a indice, lo cual hacemos en la solucion con la funcion recursiva check. Esta funcion toma por parametros el indice por el que va el analisis, el trozo de cadena que falta por analizar y las posiciones de Fibonacci que faltan por ver. Primeramente chequea si estamos en una posicion de fibonacci comparando el indice con el primer elemento d la lista d Fibonacci que le fue pasada.

Si estamos en una posicion de Fibonacci entonces, de no ser el primer caracter sin analizar una A, no se cumple la propiedad buscada, en caso de serlo, recursivamente pasamos a analizar el indice siguiente, con el resto de la cadena sin esta A y el arreglo con el resto de las posiciones d Fibonacci sin contar esta.

Si no estamos en una posicion de Fibonacci, de ser la primera letra de la cadena sin analizar una A, entonces detenemos la ejecucion y no se cumple la propiedad. De otra forma pasamos a analizar recursivamente el sgte indice y el resto de la cadena.

El caso base seria cuando no queda cadena por analizar, caso en el cual afirmamos que la propiedad se cumple.


## Fibonacci Sequence
https://dmoj.ca/problem/fibonacci

Este problema pudiera parecer sencillo, generar el n-esimo numero de Fibonacci pero su dificultad radica en el tamanho de n, que puede llegar a ser hasta 10^19.

Luego, lo primero es darnos cuentas que necesitamos un algoritmo logaritmico para calcular el n-esimo termino de la sucesion de Fibonacci. Elegi usar aquel basado en las formulas recursivas

F(2n - 1) = F(n)^2 + F(n - 1)^2
F(2n) = F(n)*(2*F(n - 1) + F(n))

Pero con esta formula no alcanza, puesto que esta formula genera un arbol de recursion, cuya cantidad de nodos crece en potencias de dos en cada division que se haga; y dado que el logaritmo en base 2 de 10^19 es mayor que 50; para un numero de ese orden el arbol tendria mas de 2^50 nodos, no terminando en tiempo el algoritmo.

No obstante; si simulamos el proceso(ya sea con python como hice, o el propio Haskell) podremos darnos cuenta de que hay numeros puntuales para el cual el calculo de Fibonacci es requerido muy frecuentemente. Luego, usando la tecnica de memization podemos guardar el valor del calculo de Fibonacci para estos numeros. Tras aplicar memoization; y teniendo precomputados los primeros 1000 numeros de Fibonacci (en mi caso); si logramos que nuestro algoritmico sea logaritmico, entrando perfectamente en tiempo


Para la implementacion genere los primers 1000 elementos de la sucesion de Fibonacci usando una funcion sencilla para la generacion de la sucesion de Fibonacci. Luego, cree la funcion que se encargaria de responder las querys. Esta funcion toma el indice soliciado y una lista con los primeros numeros d Fibonacci asi como un diccionario con los valores de Fibonacci de indices realmente grande ya computados.

El valor de devuelto es una tupla, cuyo primer elemento es la respuesta, mientras que el segundo es un diccionario con todos los nuevos calculos realizados en el. Devolvemos asi, porque al no poder actualizar variables en Haskell, el diccionario original no admitira nuevos calculos; por lo cual debemos evolver un diccionario nuevo.

Luego, este fast_fib solo consiste en, si ya tenemos calculado el valor de n, devolverlo, de otra manera, calcularlo de manera recursiva usando las formulas que se afean un poco debido al uso de los modulos; y crear nuevos diccionarios donde guardar todos los nuevos calculos que realicen las llamadas recursivas que hagamos; para luego devolver este diccionario mas sabio.


En la implementacion se hacen necesarias unas cuantas librerias externas, aquellas que contienen a Map, la que contiene al tipo Maybe, que es el tipo de devuelto de la funcion lookup que hace query al Map; la libreria Data.Bits debido al uso que hago de shiftR para acelerar la division.
Ademas senhalar que en la linea 9 uso Prelude.take en lugar de take porque de otra forma el compilador del juez online me senhala que ocurre ambiguedad al interpretar take


## Counting Permutations
Este es un ejercicio de Programacion Dinamica clasico. La formula recursiva la encontre mirando las tablas de valores generados a lo bruto por un programa que hice en python. Una vez que haya logrado aceptar el problema con Haskell buscare el porque la formula funciona

Haskell y la libreria Data.Array resultan ideales para este tipo de ejercicio de Programacion Dinamica donde primeramente se genera una tabla con todos los valores y luego tomar el valor requerido.
La solucion entra en tiempo perfectamente, no obstante falla en el caso 7 (comenzando a aparecer numeros grandes) por un error del tipo ERROR - Garbage collection fails to reclaim sufficient space.
Luego regresare a este problema