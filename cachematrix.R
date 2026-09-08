## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set the matrix, get the matrix, 
## set the cached inverse, and get the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse is already cached and the matrix is unchanged, it retrieves 
## the inverse from the cache directly, avoiding repeated computation.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
