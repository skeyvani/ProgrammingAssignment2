## Author: Siamak Keyvani
## Programming Assignment 2

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(solve) cachedInverse <<- solve
    getInverse <- function() cachedInverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedInverse <- x$getInverse()
    if(!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }
    data <- x$getMatrix()
    cachedInverse <- solve(data, ...)
    x$setInverse(cachedInverse)
    cachedInverse    
}
