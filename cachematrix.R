## There are two functions in this code that cache the inverse of a matrix.
## One is makeCacheMatrix and another is cacheSolve

## makecacheMatrix function creates a special matrix object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    getInverse <- function() {
        i
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special matrix returns by 
## makeCacheMatrix. If the inverse is already computed then the cacheMatrix 
## function retrieves the inverse from cache. 

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
