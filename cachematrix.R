## Asignación de programación 2
## Se pretende crear una función que calcule la matriz inversa si y solo si previamente no la ha calculado
## Se requieren dos funciones:
## a) una que arme la estructura para el cálculo de la matriz inversa, almacenándola en caché y
## b) una que la calcule o la extraiga del caché, dependiendo de si la ha o no calculado previamente 
## Este archivo contiene la solución (las dos funciones) 
## Arma la estructura necesaria y calcula matriz inversa 
makeCacheMatrix <- function(m = matrix()) {
	creada <- NULL  ## NULL si es la primera ejecución o
				## el nombre de la función en sucesivas 	
	set <- function(y) { ## guarda parámetros en caché 
		m <<- y	
		creada <<- NULL
		}
	get <- frunction() m 	## obtiene la matriz
	setinverse <- function(solve) creada <<- solve 
	getinverse <- function() m
   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)	    
}
## Invoca el cálculo de la matriz inversa si no la tiene en memoria o la recuupera en caso de haberla calculado previamente 
## Básicamente se tienen dos compuias de la matriz en memoria... 
cacheSolve <- function(m, ...) {
	creada <- m$getinverse() ## obtiene función o NULL
	if(!is.null(creada)) {   ## si ya había calculado la matriz 
		message("obteniendo la matriz inversa desde el caché") 
		return(creada) ## regresa el caché
		}
	datos <- m$get() ## obtiene matriz orginal 
	m <- solve(datos,...) ## calcula al inversa 
	m$setinverse(m)## la guarda en caché
	m ##regresa la inversa
}