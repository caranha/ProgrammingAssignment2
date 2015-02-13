## Programming assignment 2, by Claus Aranha
##
## This is the solution for programming assignment 2 from the "R Programming" course
## https://class.coursera.org/rprog-011
##
## The goal of the assignment is to create a function that calculates the inverse 
## of a matrix, and caches the result. If this function is invoked again on a matrix that 
## has been calculated before, then the cached result is returned.


## Creates an special object containing a matrix and a cache for 
## its inverse. Mostly an adaptation of makeVector from the 
## example, which does pretty much the same thing.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # Cached result
    set <- function(y) {
        x <<- y
        inverse <<- NULL # resets cached result when storing data
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of a matrix stored in the object created 
## by makeCacheMatrix. Again, an adaptation from the example with
## 'mean()' replaced by 'solve()'
cacheSolve <- function(x, ...) {
    ret <- x$getinverse()
    if (!is.null(ret)) { # tests if the cache exists and returns it.
	return(ret)
    }
    data <- x$get()
    ret <- solve(data,...) # calculate inverse, assumes matrixs is inversible
    x$setinverse(ret) # stores the result
    ret 
}
