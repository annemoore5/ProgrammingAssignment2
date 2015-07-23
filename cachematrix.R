## This function creates a special matrix object that can have it's inverse cached. 
## The special matrix object has 3 functions assigned to it (get, setinverse and getinverse).

## This function defines the get, setinverse, and getinverse functions on the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        get <- function() x
        setinverse <- function(mi) matinv <<- mi
        getinverse <- function() matinv
        list(get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will return the inverse of the input matrix.  
## If the inverse has not been set yet it will set it otherwise it will retrieve the inverse that had been set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinverse()
        if (!is.null(matinv)) {
                message("getting cached inverse")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinverse(matinv)
        matinv
}
