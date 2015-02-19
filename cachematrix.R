## Computing an inverse of a matrix can be costly, so caching the inverse of the 
## matrix rather than computing it repeatedly will improve performance. 
## Two functions makeCacheMarix and cacheSolve are written for this purpose.

## makeCacheMatrix has functions defined in it to get and set a matrix
## and the inverse of it. <<- operator is used to assign a value to x 
## in an environment that is different from the current environment. Here it is 
## the object x in the enclosing function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve function computes the inverse of a matrix. If the inverse 
## has already been cacluated and matrix has not changed then the cached value 
## is returned. The input matrix should be an invertible matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    inv
}
