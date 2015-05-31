# ProgrammingAssignment2
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than ## compute it repeatedly.
## Here's a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_x <<- inverse
        getInverse <- function() inv_x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_x <- x$getInverse()
        if(!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        }
        inv <- x$get()
        inv_x <- solve(inv, ...)
        x$setInverse(inv_x)
        inv_x      
}