## Caching the Inverse of a Matrix
##

## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x
        setInverseM <- function(inverse) inverseM <<- inverse
        getInverseM <- function() inverseM
        list(set = set, get = get,
             setInverseM = setInverseM,
             getInverseM = getInverseM)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverseM()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverseM(m)
        m
}