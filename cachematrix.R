## Programming Assignment 2

## A function should be created which calculates the inverse of a matrix if and only if it has not been previously calculated 

## Two functions are required:

## a) sets up the calculation "structure" for the inverse matrix and for storing it in cache and 
## b) extracts the inverse matrix from cache OR calculates the inverse and stores it in cache,
##    depending on whether or not it has been previously calculated

## This file contains the complete solution i.e.: both functions

## Calling sequence to validate

##             m1 <- originalmatrix
##             v  <- makeCacheMatrix(m1)
##             m2 <- cacheSolve(v)
##             m1 %*% m2 should give an identity matrix     

## makeCacheMatrix 
##           Sets up the necessary structure including calculation of the inverse 

makeCacheMatrix <- function(matriz = matrix()) {

    creada <- NULL  ## NULL if it is the first execution

	set <- function(y) {       ## store the matrix in cache 
		matriz <<- y	
		creada <<- NULL
		}

	get <- function() matriz 		## obtain the matrix

    
	setinverse <- function(solve) creada <<- solve  ## inverse calculation and storage
 
	getinverse <- function() creada   ## get inverse matrix  

   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## returns the vector with the functions and original matrix
   
}

## cacheSolve 
##        perform inverse matrix calculation if it is not in cache. Otherwise, recover from cache

cacheSolve <- function(vector, ...) {

	creada <- vector$getinverse() ## obtain matrix or NULL

	if(!is.null(creada)) {   ## if inverse already calculated  
		message("getting inverse matrix from cache") 
		return(creada)      ## get it from cache and return it as result
		}
	
	datos <- vector$get() 		## get original matrix  
	
	creada <- solve(datos, ...) 	## determine the inverse  
	
	vector$setinverse(creada)	## and store it in cache 

	creada					##return inverse matrix as the result
}