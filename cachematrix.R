## Asignación de programación 2

## Se pretende crear una función que calcule la matriz inversa si y solo si previamente no la ha calculado

## Se requieren dos funciones:

## a) una que arme la estructura para el cálculo de la matriz inversa, almacenándola en caché y

## b) una que la extraiga del caché o que la calcule y la almacene en caché, dependiendo de si la ha o no calculado previamente 

## Este archivo contiene la solución (las dos funciones) 

## makeCacheMatrix 
##           Arma la estructura necesaria y calcula matriz inversa 

makeCacheMatrix <- function(matriz = matrix()) {

    creada <- NULL  ## NULL si es la primera ejecución o la matriz en sucesivas 	

	set <- function(y) {       ## guarda la matriz en caché 
		matriz <<- y	
		creada <<- NULL
		}

	get <- function() matriz 		## obtiene la matriz

	setinverse <- function(solve) creada <<- solve  ## calcula inversa 
 
	getinverse <- function() creada   ## extrae matriz inversa 

   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## regresa el vector con las funciones y la matrriz original
   
}

## cahceSolve 
##        Invoca el cálculo de la matriz inversa si no la tiene en memoria o la recuupera en caso de haberla calculado previamente 

## Aún cuando esta mecánica resulta intresante, básicamente se tienen dos compias de la matriz en memoria, lo cual puede ser un problema si la memoria no es muy grande y las matrrices sí... 

cacheSolve <- function(vector, ...) {

	creada <- vector$getinverse() ## obtiene matriz o NULL

	if(!is.null(creada)) {   ## si ya había calculado la matriz 
		message("obteniendo la matriz inversa desde el caché") 
		return(creada)      ## regresa el resultado en caché
		}
	
	datos <- vector$get() 		## obtiene matriz orginal 
	
	creada <- solve(datos,...) 	## calcula al inversa 
	
	vector$setinverse(creada)			## la guarda en caché

	creada					##regresa el resultado la matriz inversa
}