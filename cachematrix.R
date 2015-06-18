## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    set <- function(y) {
        x <<- y
        M <<- NULL
    }
    get <- function() x
    
    setINV <- function(solve) M <<- solve
    getINV <- function() M
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getINV()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
      inv <- solve(data, ...)
    x$setINV(inv)
   
    return(inv)
}
