## Asignaci�n de programaci�n 2

## Se pretende crear una funci�n que calcule la matriz inversa si y solo si previamente no la ha calculado

## Se requieren dos funciones:

## a) una que arme la estructura para el c�lculo de la matriz inversa, almacen�ndola en cach� y

## b) una que la extraiga del cach� o que la calcule y la almacene en cach�, dependiendo de si la ha o no calculado previamente 

## Este archivo contiene la soluci�n (las dos funciones) 

## makeCacheMatrix 
##           Arma la estructura necesaria y calcula matriz inversa 

makeCacheMatrix <- function(matriz = matrix()) {

    creada <- NULL  ## NULL si es la primera ejecuci�n o la matriz en sucesivas 	

	set <- function(y) {       ## guarda la matriz en cach� 
		matriz <<- y	
		creada <<- NULL
		}

	get <- function() matriz 		## obtiene la matriz

	setinverse <- function(solve) creada <<- solve  ## calcula inversa 
 
	getinverse <- function() creada   ## extrae matriz inversa 

   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## regresa el vector con las funciones y la matrriz original
   
}

## cahceSolve 
##        Invoca el c�lculo de la matriz inversa si no la tiene en memoria o la recuupera en caso de haberla calculado previamente 

## A�n cuando esta mec�nica resulta intresante, b�sicamente se tienen dos compias de la matriz en memoria, lo cual puede ser un problema si la memoria no es muy grande y las matrrices s�... 

cacheSolve <- function(vector, ...) {

	creada <- vector$getinverse() ## obtiene matriz o NULL

	if(!is.null(creada)) {   ## si ya hab�a calculado la matriz 
		message("obteniendo la matriz inversa desde el cach�") 
		return(creada)      ## regresa el resultado en cach�
		}
	
	datos <- vector$get() 		## obtiene matriz orginal 
	
	creada <- solve(datos,...) 	## calcula al inversa 
	
	vector$setinverse(creada)			## la guarda en cach�

	creada					##regresa el resultado la matriz inversa
}