## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes as input a matrix and creates a special matrix 
## with a list of functions for getting, setting the matrix values
## and to store the cached value of its inverse so that the costly
## inverse operation need not be done more than once

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL                 #initialise the"inverse" matrix variable
    
    ## Stores the value of the matrix in local variable "x" 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## returs the value of the stored matrix
    get <- function() x
    
    ## caches the value of inverse of matrix
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    ## returns a list of named functions for carrying out special operations
    list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## Write a short comment describing this function
## cacheSolve takes a special matrix as input and calculates the 
## inverse of the matrix if not previously calculated
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(d, ...) {
        
    ## check if inverse has already been cached
    inv <- d$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- d$get()
    
    ## calculate the inverse of matrix using "solve" function
    inv <- solve(data, ...)
    ## cache the value of inverse
    d$setinv(inv)
    inv
    
}
