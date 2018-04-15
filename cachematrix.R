## Functions for R Programming Assignment 2 
## Functions creates a special "matrix" object that can cache its inverse and 
## calculate inverse or retrieves from cache. Only works on invertble matrices.

## Create a special "matrix" object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Compute inverse of special "matrix" returned by function above. 
## If the inverse has alrejady been calculated, then retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
