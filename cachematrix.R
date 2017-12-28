## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Set the value of the matrix
    set <- function(y) {    
        x <<- y    
        m <<- NULL 
    }
    # Get the value of the matrix
    get <- function() x
    # Set the inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
    # Get the inverse of the matrix
    getInverse <- function() m
    
    # Return a list with the above four functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x) {
    ## Check if there is already a calculated inverse matrix
    m <- x$getInverse() 
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    ## If it does not exist calculate the inverse matrix
    data <- x$get()  
    m <- solve(data)
    x$setInverse(m)  
    m               
}
