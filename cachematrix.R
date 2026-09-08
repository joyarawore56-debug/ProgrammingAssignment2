## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It initializes the inverse cache to NULL and returns a list of functions:
## 1. set: updates the matrix value and resets the cached inverse to NULL (clearing the cache)
## 2. get: retrieves the current matrix value
## 3. setinverse: stores/caches the calculated inverse matrix
## 4. getinverse: retrieves the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set a new matrix and reset the cache because the matrix changed
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Return the stored matrix
    get <- function() x
    
    # Store the inverse in the parent environment
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Retrieve the cached inverse
    getinverse <- function() {
        inv
    }
    
    # Return the list containing all four functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## It first checks if the inverse has already been cached. If a cached value exists 
## and the matrix hasn't changed, it returns the cached data directly to skip computation.
## Otherwise, it calculates the inverse using solve(), stores it in the cache, and returns it.
cacheSolve <- function(x, ...) {
    ## Attempt to retrieve the cached inverse from the special matrix object
    inv <- x$getinverse()
    
    # If the cache is not empty, return the cached inverse immediately
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If the cache is empty, get the matrix data
    data <- x$get()
    
    # Compute the matrix inverse using the solve function
    inv <- solve(data, ...)
    
    # Cache the newly computed inverse back into the special matrix object
    x$setinverse(inv)
    
    # Return the inverse matrix
    inv
}
