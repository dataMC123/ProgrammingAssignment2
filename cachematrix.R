## The following functions cach the inverse of a matrix rather than 
## computing it repeatedly, in the case the matrix inversion has been
## done already

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    ## variable initialization
    inv <- NULL
    
    ## function definitions to set and get the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    ## function definitions to set and get the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## get the inverse of the matrix x, if already computed
    ## returned the stored inverse matrix and return 
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if inverse matrix has not been computed get the x matrix data
    data <- x$get()
    ## and compute the inverse matrix
    inv <- solve(data, ...)
    ## then cache the inverse matrix
    x$setinv(inv)

    ## Return a matrix that is the inverse of 'x'
    inv
}
