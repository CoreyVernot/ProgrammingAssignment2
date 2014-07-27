## 2 functions. makeCacheMatrix creates a matrix object
## and returns a list of functions. cacheSolve calculates the
## inverse of the matrix, and will find and return a cached
## matrix if the inverse has already been calculated.

## makeCacheMatrix makes a matrix and returns a list
## of four functions to 1) set the matrix 2) get the
## matrix 3) set the inverse of the matrix and 4) get
## the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set= set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    
}

##cacheSolve is a function that calculates the inverse of
## a matrix. If the inverse has already been calculated
## and saved as inv, the function returns inv instead of
## calculating.

cacheSolve <- function(x, ...) {
    inv <- x$getmean()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}