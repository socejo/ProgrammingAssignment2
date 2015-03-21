## Programming Assignment 2

## A function should be created which calculates the inverse of a matrix if and only if it has not been previously calculated 

## Two functions are required:

## a) one that sets up the calculation "structure" for the inverse matrix, stroing it in cache and 
## b) one that estracts the inverse matrix from cache OR calculates the inverse and stores it in cache,
##    dependint on wheter or not it has been previously calculated

## This file contains the complete solution i.e.: both functions

## Calling secuence 

##             v1 <- makeCachematrix(originalmatrix)
##             v2 <- cacheSolve(V1)
##             v1 %*% v2 should give an identity matrix     

## makeCacheMatrix 
##           Sets up the necesary struicture including calculation of the inverse 

makeCacheMatrix <- function(matriz = matrix()) {

    creada <- NULL  ## NULL if it is the first execution

	set <- function(y) {       ## store the matrix in cache 
		matriz <<- y	
		creada <<- NULL
		}

	get <- function() matriz 		## obtain the matriz

	setinverse <- function(solve) creada <<- solve  ## inverse calculation 
 
	getinverse <- function() creada   ## get inverse matrix  

   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## regresa the vector with the functions and original matrix
   
}

## cacheSolve 
##        perform inverse matrix calculation if it is not in cache. Otherwise recover from cache

## Eventhough this structure ois very interesting, you finish with two copies of the matrix in memory 
## which can be a problem if youy don't have big memory available

cacheSolve <- function(vector, ...) {

	creada <- vector$getinverse() ## obtain matrix or NULL

	if(!is.null(creada)) {   ## if inverse already calculated  
		message("getting inverse matrix from cache") 
		return(creada)      ## get it from cache and return result
		}
	
	datos <- vector$get() 		## get original matrix  
	
	creada <- solve(datos,...) 	## determine the inverse  
	
	vector$setinverse(creada)	## and storew it in cache 

	creada					##return inverse matrix as the result
}