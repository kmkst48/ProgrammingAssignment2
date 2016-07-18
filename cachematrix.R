## This file defines a 'cacheMatrix' type, which
## wraps an ordinary matrix object and
## adds the ability to cache that matrix's inverse.


## makeCacheMatrix creates a new cacheMatrix object
## based on an existing R matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function (newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(inv) inverse <<- inv 
    
    list (set = set, 
          get = get, 
          getInverse = getInverse, 
          setInverse = setInverse)
}


## Returns the inverse of a given cacheMatrix.
## If the inverse has not already been cached, this
## function will cache it by storing the inverse in x.
cacheSolve <- function(x, ...) {

    ## Check for a cached inverse in x
    inverse <- x$getInverse()
    if (! is.null(inverse)) {
        message("returning cached inverse")
        return (inverse)
    }
    
    ## Calculate the inverse of x's matrix
    ## and cache it within x.
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
