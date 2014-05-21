## Pair of functions that cache the inverse of a given matrix

## Creates a "special matrix object" which contains
## a list of specific functions for it

makeCacheMatrix <- function(x = matrix()) {
    
    ## initializing empty inverse of the matrix object
    invx <- NULL
    
    ## set the value of the matrix object
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    
    ## returns the matrix object
    get <- function() x
    
    ## returns the inverse of the matrix object
    getinverse <- function() invx
    
    ## sets the iverse of the matrix object
    setinverse <- function(inverse) invx <<- inverse
    
    ## returning of the list containing the object's functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## Calculates and caches the inverse of the given
## "special matrix object"

cacheSolve <- function(x, ...) {
    
    ## return a matrix that is the inverse of 'x'
    invx <- x$getinverse()
    
    ## checking if the inverse was cached already and return it
    if(!is.null(invx)) {
        message("getting cached inverse of the matrix")
        return(invx)
    }
    
    ## getting the given "special matrix object", inversing and caching
    mtrx <- x$get()
    invx <- solve(mtrx)
    x$setinverse(invx)
    
    ## return the inverse
    invx
}
