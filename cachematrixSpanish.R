## Asignaci�n de programaci�n 2

## Se pretende crear una funci�n que calcule la matriz inversa si y solo si previamente no ha sido calculada

## Se requieren dos funciones:

## a) armar la estructura para el c�lculo de la matriz inversa y para almacenarla en cach� y

## b) extraerla del cach� o calcularla y almacenarla en cach�, dependiendo de si la ha o no calculado previamente 

## Este archivo contiene la soluci�n (las dos funciones) 

## Secuencia de ejecuci�n para validar

##             m1 <- originalmatrix
##             v  <- makeCacheMatrix(m1)
##             m2 <- cacheSolve(v)
##             m1 %*% m2 debe dar la matriz dentidad     

## makeCacheMatrix 
##           Arma la estructura necesaria incluyendo el c�lculo de la matriz inversa 

makeCacheMatrix <- function(matriz = matrix()) {

    creada <- NULL  ## NULL si es la primera ejecuci�n  	

	set <- function(y) {       ## guarda la matriz en cach� 
		matriz <<- y	
		creada <<- NULL
		}

	get <- function() matriz 		## obtiene la matriz

	setinverse <- function(solve) creada <<- solve  ## calcula y almacena la inversa 
 
	getinverse <- function() creada   ## extrae matriz inversa 

   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## regresa el vector con las funciones y la matrriz original
   
}

## cahceSolve 
##        Invoca el c�lculo de la matriz inversa si no la tiene en cach� o la recuupera en caso de haberla calculado previamente 

cacheSolve <- function(vector, ...) {

	creada <- vector$getinverse() ## obtiene matriz o NULL

	if(!is.null(creada)) {   ## si ya hab�a calculado la matriz inversa
		message("obteniendo la matriz inversa desde el cach�") 
		return(creada)      ## regresa el resultado en cach�
		}
	
	datos <- vector$get() 		## obtiene matriz orginal 
	
	creada <- solve(datos,...) 	## calcula la inversa 
	
	vector$setinverse(creada)			## la guarda en cach�

	creada					##regresa como resultado la matriz inversa
}