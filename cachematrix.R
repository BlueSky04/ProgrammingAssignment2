## A function to cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    IN <- NULL
    set <- function(y) {
        x <<- y
        IN <<- NULL
        }
    get <- function()  x
    setInversion <- function(solve) IN <<- solve
    getInversion <- function() IN
    list (set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        IN <- x$getInversion()
        if(!is.null(IN)){
            message("getting cached data")
            return(IN)
        }
        data <- x$get()
        IN <- solve(data, ...)
        x$setInversion(IN)
        IN
}
