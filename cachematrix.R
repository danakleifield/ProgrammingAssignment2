
## This function defines the accessor and mutator functions ('get' and 'set')
## for both a matrix object and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInv <- function() invM <<- solve(x)
    getInv <- function() invM
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function checks for whether a valid result-- the matrix's inverse-- exsits in the cache, and returns it.
## If not, it inverts the matrix and returns the result, using the functions defined in makeCacheMatrix().

cacheSolve <- function(x, ...) {
    invM <- x$getInv()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setInv(invM)
    invM
}
