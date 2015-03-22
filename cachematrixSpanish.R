## Asignación de programación 2

## Se pretende crear una función que calcule la matriz inversa si y solo si previamente no ha sido calculada

## Se requieren dos funciones:

## a) armar la estructura para el cálculo de la matriz inversa y para almacenarla en caché y

## b) extraerla del caché o calcularla y almacenarla en caché, dependiendo de si la ha o no calculado previamente 

## Este archivo contiene la solución (las dos funciones) 

## Secuencia de ejecución para validar

##             m1 <- originalmatrix
##             v  <- makeCacheMatrix(m1)
##             m2 <- cacheSolve(v)
##             m1 %*% m2 debe dar la matriz dentidad     

## makeCacheMatrix 
##           Arma la estructura necesaria incluyendo el cálculo de la matriz inversa 

makeCacheMatrix <- function(matriz = matrix()) {

    creada <- NULL  ## NULL si es la primera ejecución  	

	set <- function(y) {       ## guarda la matriz en caché 
		matriz <<- y	
		creada <<- NULL
		}

	get <- function() matriz 		## obtiene la matriz

	setinverse <- function(solve) creada <<- solve  ## calcula y almacena la inversa 
 
	getinverse <- function() creada   ## extrae matriz inversa 

   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## regresa el vector con las funciones y la matrriz original
   
}

## cahceSolve 
##        Invoca el cálculo de la matriz inversa si no la tiene en caché o la recuupera en caso de haberla calculado previamente 

cacheSolve <- function(vector, ...) {

	creada <- vector$getinverse() ## obtiene matriz o NULL

	if(!is.null(creada)) {   ## si ya había calculado la matriz inversa
		message("obteniendo la matriz inversa desde el caché") 
		return(creada)      ## regresa el resultado en caché
		}
	
	datos <- vector$get() 		## obtiene matriz orginal 
	
	creada <- solve(datos,...) 	## calcula la inversa 
	
	vector$setinverse(creada)			## la guarda en caché

	creada					##regresa como resultado la matriz inversa
}