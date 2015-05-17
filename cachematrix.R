## cachematrix.R is a set of utility functions that can be used to store and retrieve the inverse of a matrix using a memory cache 
## rather than recomputing it.  For an example of how to use these functions, consider the invertible matrix:
##
## m <- matrix(1:4, ncol = 2, ncol = 2)
## solve(m)  # the inverse of m
##
## The inverse of matrix m can be calculated with solve(m).  The cachematrix alternative would be:
##
## m <- matrix(1:4, ncol = 2, ncol = 2)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)   # the inverse of m

## makeCacheMatrix creates an object for a given matrix that includes methods to cache the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)   
}

## cacheSolve takes a makeCacheMatrix object and uses its methods to check to see if the inverse has been cached first before computing it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
