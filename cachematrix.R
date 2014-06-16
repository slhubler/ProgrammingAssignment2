## This module creates a way to cache inverted matrices
## It uses two functions makeCacheMatrix and cacheSolve.
## Example of use:
##      mm = matrix(rnorm(n=10000),nrow=100)
##      mc = makeCacheMatrix(mm)
##      result1 = cacheSolve(mc)
##      result2 = cacheSolve(mc) # second time is faster

## Creates list of functions that are specific to the 
##      input matrix
## Names of functions in list:
##      set -- sets the original matrix
##      get -- gets the original matrix
##      setsolve -- sets result of "solving"
##      getsolve -- gets result of solving
##              (or NULL if not set yet)

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


## "Solves" matrix if not done yet.
## Otherwise retrieves original result

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
