# ==============================================================================
# Script Name: cachematrix.R
#
# Author: Charlie Letts
# Date: 15/02/2025
#
# Purpose: This script allows you to cache a given matrix as well as the inverse
# of that matrix. If the inverse has not already been calculated, then 
# cacheSolve() will calculate this, cache the result, and return the result.
# Otherwise, it will just return the cached value.
# ==============================================================================

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x_inverse <<- inverse
        getinverse <- function() x_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function either returns the cached inverse, if the cached value is not NULL,
## otherwise it will calculate and cache the inverse before returning it.

cacheSolve <- function(x, ...) {
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        matrix <- x$get()
        x_inverse <- solve(matrix)
        x$setinverse(x_inverse)
        x_inverse
}

