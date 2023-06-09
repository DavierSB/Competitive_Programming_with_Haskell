## Alice Through the Looking Glass
https://dmoj.ca/problem/ccc11s3

En este problema Alicia tiene una imagen extraida de su microscopio. Esta dividida en cuadriculas de 5x5. Cada grado de magnificacion divide cada una de las cuadriculas en otra tabla cuadriculada de 5x5. En cada cuadricula, luego de magnificada la imagen puede aparecer hielo del copo de nieve que Alicia esta analizando o no. Nos piden que dados un par de coordenadas y el nivel de magnificacion de la imagen determinemos si en esa posicion se encuentra hielo o no. Por supuesto, sabemos algo sobre el copo de nieve, y es que tiene una forma fractal, cada copo ocupa tres cuadrantes en la base, uno en su segundo nivel, y cada uno de los cuadrantes en los extremos de la base, asi como el superior cumplen que directamente encima de ellos se encuentra otro una figura similar pero a mucho menor escala. En el enunciado del problema podremos ver con mucho mas detalle este patron 

La solucion a este problema es recursiva. Dado el nivel de magnificacion m y las coordenadas (x, y), lo primero seria encontrar en cual cuadrante de una division (5x5) se encuentran las coordenadas actuales. Para ello nos quedamos con las partes enteras de x/(5^(m-1)) y y/(5^(m-1)) ya que cada cuadrante tiene lado 5^(m - 1)

Tambien es util conocer las coordenadas que tiene (x, y) dentro del cuadrante en que se encuentra. Para ello nos quedamos con el resto de las divisiones x/(5^(m-1)) e y/(5^(m-1)). Llamemos a y b a estas nuevas coordenadas

Sabiendo el cuadrante en que se encuentra (x, y) podemos clasificarla de la sgte manera:

   1 - Si el cuadrante esta en la primera o ultima columna, o en la segunda o cuarta columnas por encima de la segunda fila, o en la tercera columna en una de la ultimas dos filas, podemos clasificar con seguridad las coordenadas como vacias.

   2 - Si el cuadrante esta en la primera fila en una de las columnas centrales, o esta ubicada en la columna central de la segunda. En tal caso podemos clasificar con seguridad que estas coordenadas corresponden a un cristal

   3 - Los casos restantes corresponden a cuadrantes inmediatamente encima de cristal, pero no son completamente cristal. Esto significa que debemos mirar dentro de estos cuadrantes para determinar el estado de las coordenadas. Luego, llamamos a resolver de manera recursiva el problema para magnificacion (m - 1) y coordenadas (a, b).
   
   El caso base seria cuando el nivel de maginifacicion sea 0. Si llegamos alli querra decir que en el paso anterior, nivel de magnificacion 1, nos encontrabamos en una de los cuadrantes que entran dentro de la clasificacion expuesta en el punto 3, o sea que no son por completo cristal, pero como no puede ser dividido mas, entonces no puede contener cristal en lo absoluto el cuadrante actual, de donde la casilla esta vacia

## Bananas
https://dmoj.ca/problem/ccc05j5

En este sencillo problema, debemos comprobar si la entrada (una palabra) cumple con ciertas reglas. Recursivamente, vamos letra a letra comprobando que la letra analizada en cada momento no rompa las reglas. Para ello, a caa letra recursiva debemos pasarle dos variables que reflejan el estado de la cadena analizada hasta este momento y ellas son, si exste una B abierta anteriormente (o sea no se le ha colocado su S), y si ahora debemos encontrar un caracter de terminacion de palabra (N o S) o no.
Una vez hayamos analizado toda la cadena llamaremos a la funcion sobre la palabra "". Este sera el caso base cuyo valor de retorno sera verdadero en caso de que no haya B abiertas y que corresponda iniciar una nueva palabra (señal de que la anterior ya esta cerrada)


## Bruno and Fibonacci
https://dmoj.ca/problem/gfsspc1p4

En este problema debemos comprobar si una cadena dada cumple que tiene letras A solo en las posiciones correspondientes a indices que estan en la sucesion de Fibonacci.

Dado que la longitud de la cadena no sobrepasa 500 entonces no necesitamos mas alla de los primeros 15 valores diferentes de Fibonacci

Luego, el analizar si la cadena tiene a la A solo en posiciones con indices de Fibonacci, se puede hacer de manera recursiva, analizando indice a indice, lo cual hacemos en la solucion con la funcion recursiva check. Esta funcion toma por parametros el indice por el que va el analisis, el trozo de cadena que falta por analizar y las posiciones de Fibonacci que faltan por ver. Primeramente chequea si estamos en una posicion de fibonacci comparando el indice con el primer elemento d la lista d Fibonacci que le fue pasada.

Si estamos en una posicion de Fibonacci entonces, de no ser el primer caracter sin analizar una A, no se cumple la propiedad buscada, en caso de serlo, recursivamente pasamos a analizar el indice siguiente, con el resto de la cadena sin esta A y el arreglo con el resto de las posiciones d Fibonacci sin contar esta.

Si no estamos en una posicion de Fibonacci, de ser la primera letra de la cadena sin analizar una A, entonces detenemos la ejecucion y no se cumple la propiedad. De otra forma pasamos a analizar recursivamente el sgte indice y el resto de la cadena.

El caso base seria cuando no queda cadena por analizar, caso en el cual afirmamos que la propiedad se cumple.


## Cyclic Shifts
https://dmoj.ca/problem/ccc20j4

Una rotacion de una cadena es cuando desplazamos todos los caracteres de una cadena 1 indice hacia delante (o hacia atras), con el correspondiente desplazamiento del primer caracter hacia la ultima posicion (o viceversa). Por ejemplo

cadena, adenac, denaca, enacad, nacade, acaden

son todas rotaciones de la palabra cadena


En este problema, la entrada consiste en dos cadenas T y s y nuestra tarea es descubrir si en la cadena T aparece alguna rotacion de la cadena s

Para solucionar este probema usando el paradigma de la programacion declarativa con Haskell, definimos la funcion shifts, que devuelve la sucesion de las rotaciones de una cadena. Definimos ademas la funcion get_substrings que nos devuelve todas las subcadenas de un tamaño determinado de una cadena. Luego, solo nos queda encontrar si hay alguna cadena que es tanto una rotacion de s como una subcadena de T y por tanto es comun en las listas producidas por shifts s y por get_substrings T length s.

La complejidad de esta solucion es cuadratica; pero dada las constraints del problema (cadenas de tamaño hasta 1000), pasa todos los casos de prueba sin inconvenientes


## Fibonacci Sequence
https://dmoj.ca/problem/fibonacci

Este problema pudiera parecer sencillo, generar el n-esimo numero de Fibonacci pero su dificultad radica en el tamaño de n, que puede llegar a ser hasta 10^19.

Luego, lo primero es darnos cuentas que necesitamos un algoritmo logaritmico para calcular el n-esimo termino de la sucesion de Fibonacci. Elegi usar aquel basado en las formulas recursivas

F(2n - 1) = F(n)^2 + F(n - 1)^2
F(2n) = F(n)*(2*F(n - 1) + F(n))

Pero con esta formula no alcanza, puesto que esta formula genera un arbol de recursion, cuya cantidad de nodos crece en potencias de dos en cada division que se haga; y dado que el logaritmo en base 2 de 10^19 es mayor que 50; para un numero de ese orden el arbol tendria mas de 2^50 nodos, no terminando en tiempo el algoritmo.

No obstante; si simulamos el proceso(ya sea con python como hice, o el propio Haskell) podremos darnos cuenta de que al calcular el valor del n-esimo numero de FIbonacci hay numeros puntuales para el cual el calculo de Fibonacci es requerido muy frecuentemente, mientras que la enorme mayoria de los restantes numeros entre 1000 y n son completamente ignorados (los numeros pequeños si son todos reuqeridos frecuentemente). Luego, usando la tecnica de memoization podemos guardar el valor del calculo de Fibonacci para los numeros mayores que 1000 mas requeridos. Tras aplicar memoization; y teniendo precomputados los primeros 1000 numeros de Fibonacci (en mi caso, 1000 es un numero arbitrario, pudiera ser cualquier otro); si logramos que nuestro algoritmico sea logaritmico, entrando perfectamente en tiempo

Para la implementacion generamos los primeros 1000 elementos de la sucesion de Fibonacci usando una funcion sencilla para la generacion de la sucesion de Fibonacci. Luego, cree la funcion que se encargaria de responder las querys, llamada fast_fib. Esta funcion toma el indice solicitado, una lista con los primeros numeros d Fibonacci y un diccionario con los valores de Fibonacci de indices realmente grande ya computados.

El valor de devuelto es una tupla, cuyo primer elemento es la respuesta, mientras que el segundo es un diccionario con todos los nuevos calculos realizados en el. Devolvemos asi, porque al no poder actualizar variables en Haskell, el diccionario original no admitira nuevos calculos; por lo cual debemos devolver un diccionario nuevo.

Luego, este fast_fib solo consiste en, si ya tenemos calculado el valor de n, devolverlo, de otra manera, calcularlo de manera recursiva usando las formulas -se afean un poco debido al uso de los modulos- y crear nuevos diccionarios donde guardar todos los nuevos calculos que realicen las llamadas recursivas que hagamos; para luego devolver este diccionario mas sabio.


En la implementacion se hacen necesarias unas cuantas librerias externas, aquellas que contienen a Map, la que contiene al tipo Maybe, que es el tipo de devuelto de la funcion lookup que hace query al Map; la libreria Data.Bits debido al uso que hago de shiftR para acelerar la division.
Ademas señalar que en la linea 9 uso Prelude.take en lugar de take porque de otra forma el compilador del juez online me señala que ocurre ambiguedad al interpretar take


## Golf
https://dmoj.ca/problem/ccc00s4

Roberta la Robot dispone de hasta 32 palos de golf y sus golpeos tienen siempre la misma longitud para el mismo palo de golf. Determinar la menor cantidad de golpeos que debe hacer Roberta para mover la bola hasta el hoyo que se encuentra exactamente a una distancia determinada 

Este es un problema clasico de Programacion Dinamica, el problema de devolver el vuelto con la menor cantidad de monedas posibles, teniendo en disposicion ciertos tipos de monedas.

Para resolverlo, rellenamos un array, desde la posicion 0 en adelante de la sgte manera. A la posicion 0 le asignamos valor 0 (o sea el inicio del campo de golf lo alcanzamos sin golpeo) y a las restantes -1 (no han sido alcanzadas). Luego, iteramos por todas las posiciones, y en cada posicion, si ya hemos llegado a esta posicion, entonces, desde ella podremos acceder a todas las posiciones que se encuentren (hacia delante) a una distancia identica a la que alcanzamos con determinado palo de golf. Para tales posiciones alcanzables actualizamos su valor. O sea, si no han sido alcanzadas colocamos en ella la cantidad de golpeos actual + 1. Si ya lo fueron entonces su valor correspondera al menor entre el que ya tenian y el de la cantidad de golpeos actual + 1. Paramos la iteracion al alcanzar la distancia a la que se encuentra el hoyo, cuyo valor devolvemos.

Hacer esto es realmente sencillo en Haskell valiendonos de la estructura Array


## Pi-Day
https://dmoj.ca/problem/ccc15j5

Debemos encontrar la forma de repartir K porciones de cake entre n matematicos de manera que la cantidad asignada al i-esimo matematico sea igual o mayor que la cantidad asignada al matematico (i-1)

Este problma es equivalente a la cantidad de arreglos ordenados de n elementos todos mayores que 1 que suman k. Tal cantidad es igual a la cantidad de tales arreglos que tienen un elemento de valor 1 mas la cantidad de tales arreglos cuyos elementos todos son mayores que 1. Podemos darnos cuenta de que la cantidad de arreglos de tamanho n y suma k que tienen un elemento con valor 1 es igual a la cantidad de arreglos de tamanho n-1 y suma k-1; mientras que la cantidad de arreglos d tamanho n y suma k tales que todos sus elementos son mayores que 1 es equivalente a la cantidad de arreglos de suma (k-n) y n elementos (o sea al quitarle 1 a cada elemento)

Luego, usando programacion dinamica podemos crear una tabla de dos dimensiones (n y k); donde la posicion (i, j) valdra lo que la suma de las posiciones (i - 1, j - 1) y (i, j - i). Esto es muy sencillo de hacer usando la estructura Array de la libreria Data.Array


## Primes & Pemutations
https://dmoj.ca/problem/dmopc21c5p3

Un robot comienza en un edificio de altura n. Este edificio se encuentra en un conglomerado de edificios cuyas alturas constituyen una permutacion de los numeros desde 1 hasta n. El robot solo puede saltar a edificios de menor altura que se encuentren a una distancia prima del edificio en el que se encuentra. Dos ninhos juegan a mover el robot. Pierde aquel que no pueda hacer ningun movimiento. El inventor del juego desea saber si para ciertos valores de n existen permutaciones en las que aunque los dos jugadores jueguen de manera optima siempre gana el primer ninho. Luego nos pasara una serie de casos de pruebas con tales valores de n y en caso de existir tal permutacion donde siempre puede ganar el primero, debemos devolver la menor lexicograficamente hablando.

Este es un problema de Teoria de juegos que puede ser resuelto usando programacion dinamica. O sea, para cada valor de n, encontramos si al comenzar el robot en la ultima posicion de la permutacion ordenada (que sera la menor lexicograficamente) es una posicion ganadora para el primer jugador o no. Una posicion sera ganadora si se encuentra a distancia prima de una posicion perdedora y por tanto podemos llevar el robot alli forzando al rival a encontrarse en una posicion perdedora. Las posiciones 1 y 2 son perdedoras porque no existe movimiento que se pueda realizar. La posicion 3 es ganadora porque al mover el robor dos edificios (numero primo) forzamos al rival a una posicion perdedora. Asi, el ejercicio va de rellenar un arreglo, donde vamos posicion por posicion comprobando si es una posicion perdedora o ganadora y almacenamos tal dato. Para comprobar el estado de una posicion probamos con todos los primos menores que ella y vemos si un salto de esa longitud nos lleva a una posicion ganadora. A pesar de que esto aparentemente tiene complejidad cercana a la cuadratica, en realidad, dada la predominancia de las posiciones ganadoras, sera muy inferior, puesto que para cada posicion ganadora encontrar una posicion ganadora que se encuentre a la distancia de un salto primo sera relativamente sencillo. Para precomputar estos calculos creamos un array de los brindados por la estrutura Data.Array

Luego, al procesar cada query, si el valor de la query corresponde a una posicion ganadora, entonces sencillamente podemos devolver la permutacion ordenada de menor a mayor. De no ser asi, si la posicion (n - 1) corresponde a una posicion ganadora deberemos devolver el arreglo ordenado de menor a mayor con las dos ultimas posiciones intercambiadas, de manera que la mas alta estara en la posicion (n - 1), ganadora. No pueden existir tres posiciones perdedoras consecutivas, porque como la distancia entre la tercera y la primera es 2 (nuemero primo), entonces de la primera ser perdedora, la ultima sera ganadora. Luego, si ni la posicion n ni la (n-1) son ganadoras, entonces lo sera la (n-2). Luego, devolvemos la permutacion ordenada de menor a mayor pero con una rotacion de los ultimos tres elementos de manera que queden n (n-2) (n-1).



Inconclusos

## Primes 2
https://dmoj.ca/problem/primes2

Se trata de generar e imprimir todos los primos entre dos numeros dados cuya separacion no es mayor e 5000000 y losnumeros pueden tener valor de hasta 10^9. Para solucionarlo, primeramente generaremos todos los primos hasta 31623 (techo de la raiz de 10^9). Todo numero no primo menor que 10^9 tiene un factor primo entre estos numeros. Luego el problema se reduce a cribar los numeros del intervalo con todos los primos entre 1 y 31623. Una ultima optimizacion que no hemos llevado a cabo aun va acerca de en lugar de cribar sucesivamente, ir recorriendo los numeros y para cada numer uscar entre sus posibles divisores primos hasta su raiz. De aparecer alguno, en tal caso es que realizamos la criba de los numeros multiplos de tal primo, y luego no comprobamos que este primo sea divisor de ninguno de los futuros primos a analizar. O sea, cribar de manera lazy.
 

## Counting Permutations
https://dmoj.ca/problem/mathp8

Este es un ejercicio de Programacion Dinamica clasico. La formula recursiva la encontre mirando las tablas de valores generados a lo bruto por un programa que hice en python. Una vez que haya logrado aceptar el problema con Haskell buscare el porque la formula funciona

Haskell y la libreria Data.Array resultan ideales para este tipo de ejercicio de Programacion Dinamica donde primeramente se genera una tabla con todos los valores y luego tomar el valor requerido.
La solucion entra en tiempo perfectamente, no obstante falla en el caso 7 (comenzando a aparecer numeros grandes) por un error del tipo ERROR - Garbage collection fails to reclaim sufficient space.
Luego regresare a este problema