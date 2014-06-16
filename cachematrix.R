## makeCacheMatrix() creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initially, cache is empty until first invocation of cacheSolve()
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    ## Construct and return object with accessor methods
    get <- function() x
    setinv <- function(inv) cache <<- inv
    getinv <- function() cache
    ret<-list(set = set, get = get, setinv = setinv, getinv = getinv)
    invisible(ret)
}


## cacheSolve() computes the inverse of the special "matrix" returned by
## makeCacheMatrix() above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    
    ## Check for existing cache of inverse, return it if available
    cache <- x$getinv()
    if(!is.null(cache)) {
        return(cache)
    }
    
    ## Otherwise compute inverse and then cache for next time
    data <- x$get()
    cache <- solve(data, ...)
    x$setinv(cache)
    cache
}
