## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    M <- NULL
    set <- function(y) { # set the value of the matrix
        x <<- y
        M <<- NULL
    }
    get <- function() x  # get the value of the  matrix
    
    setINV <- function(solve) M <<- solve  #set the value of the inverse
    getINV <- function() M   # #get the value of the inverse.
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getINV() ##return the cache value if encounter the x before
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    ## compute and cache the inverse of the matrix if it is not in the cache.
    data <- x$get() 
      inv <- solve(data, ...) #Compute the inverse of a square matrix by solve
    x$setINV(inv)  # cashe the soultion
   
    return(inv)
}
