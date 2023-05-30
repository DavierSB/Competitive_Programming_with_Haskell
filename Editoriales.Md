## Cyclic Shifts

https://dmoj.ca/problem/ccc20j4

Una rotacion de una cadena es cuando desplazamos todos los caracteres de una cadena 1 indice hacia delante (o hacia atras), con el correspondiente desplazamiento del primer caracter hacia la ultima posicion (o viceversa). Por ejemplo

cadena, adenac, denaca, enacad, nacade, acaden

son todas rotaciones de la palabra cadena


En este problema, la entrada consiste en dos cadenas T y s y nuestra tarea es descubrir si en la cadena T aparece alguna rotacion de la cadena s

Para solucionar este probema usando el paradigma de la programacion declarativa con Haskell, definimos la funcion shifts, que devuelve la sucesion de las rotaciones de una cadena. Definimos ademas la funcion get_substrings que nos devuelve todas las subcadenas de un tamanho determinado de una cadena. Luego, solo nos queda encontrar si hay alguna cadena que es tanto una rotacion de s como una subcadena de T y por tanto es comun en las listas producidas por shifts s y por get_substrings T length s.

La complejidad de esta solucion es cuadratica; pero dada las constraints del problema (cadenas de tamanho hasta 1000), pasa todos los casos de prueba sin inconvenientes
