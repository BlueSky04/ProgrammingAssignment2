## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
