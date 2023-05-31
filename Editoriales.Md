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
