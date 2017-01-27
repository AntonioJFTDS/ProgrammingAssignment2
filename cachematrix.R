## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## So I wrote a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mx) {
        
        minv <- NULL
        
        set <- function(my) {
                mx <<- my 
                minv <<- NULL
        }
        
        get <- function() mx
        
        setinv <- function(inv) minv <<- inv
        
        getinv <- function() minv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(mx, ...) {
        minv <- mx$getinv()
        if(!is.null(minv)) {    ## test if the inverese has already been calculated
                message("getting cached data")
                return(minv)
        }
        data <- mx$get()
        minv <- solve(data, ...)
        mx$setinv(minv)
        minv
}