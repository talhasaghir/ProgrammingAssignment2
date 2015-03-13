## The following functions create a cache structure for a matrix, and stores its inverse.
## It also calculates and stores the inverse, if it's not calculated already, and returns 
## the pre-calculated inverse value otherwise.

## the following function returns a list of functions for a specific matrix that
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverseVal) inverse <<- inverseVal
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The following function checks if the inverse of the matrix already exists in the cache. 
## If yes, it returns its value. Otherwise, it calculates the inverse, stores in the cache, 
## and returns it.

cacheSolve <- function(x, ...) {

    inverse <- x$getInverse()    
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    
    data <- x$get()
    
    inverse <- solve(data)
    
    x$setInverse(inverse)
    
    inverse
    
}
