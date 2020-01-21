## Cahing the Inverse of a Matrix

## The function makeCacheMatrix creates a special matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invcache <- NULL
    set <- function(y) {
        x <<- y
        invcache <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invcache <<- inverse
    getinv <- function() invcache
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse of the cache.
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    invcache <- x$getinv()
    if (!is.null(invcache)) {
        message("getting cached data")
        return(invcache)
    }
    data <- x$get()
    invcache <- solve(data, ...)
    x$setinv(invcache)
    invcache
}


#Test
mat <- matrix(c(1,2,3,4),2,2)
matinv <- makeCacheMatrix(mat)
cacheSolve(matinv)
