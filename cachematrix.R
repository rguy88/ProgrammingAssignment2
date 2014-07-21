## Set of functions to calculate the inverse of matrix with cache support
## To use these functions, first create a wrapper object using makeCacheMatrix()
## then call cacheSolve() with the wrapper object.
## Example: 
##      m <- makeCacheMatrix(matrix(rnorm(9), ncol=3))
##      trace(solve)
##      cacheSolve(m)
##      cacheSolve(m)
##      untrace(solve)
## In the code above, the solve() function is not called the second time when
## cacheSolve() is called, as indicated by trace()


## This function creates a wrapper object to be used with cacheSolve()
## It takes a square matrix that is inversable as parameter

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(r) inv <<- r
    getinv <- function() inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function finds the inverse of the matrix. If the inverse was calculated
## previously, it is simply returned without calculating again.
## It takes the CacheMatrix wrapper as parameter. Any additional parameters 
## for the solve() function can also be passed after the first parameter.

cacheSolve <- function(x, ...) {
    r <- x$getinv()
    if (!is.null(r)) {
        return(r)
    }

    data <- x$get()
    r <- solve(data, ...)
    x$setinv(r)
    r
}
