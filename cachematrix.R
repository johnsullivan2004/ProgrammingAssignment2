## Programming Assignment #2 for Coursera R-Programming course
## 
## John Sullivan 4/13/2017
##
## Two function that work together to cache the inverse of a matrix.
##   These functions Could be used to help speed up calculations
##
## This code uses the makeVector and cachemean example codes as a
##  starting point.
##
## The makeCacheMatrix function creates a special vector function
##   that caches a matrix and its inverse using the lexical scoping 
##   features in R using the assignment operator.
## 
##  Usage:  y <- makeCacheMatrix (X)
##     where y houses the special vector function
##     and x is the matrix
##     Must run every time the matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolver function works in tandem with the makeCacheMatrix
## to cache a matrix inverse
##
## If the matrix is uninitialled or changes, the matrix is saved,
## a new matrix inverse is calculated, cached, and returned.
##
## if the matrix is the same, the cached matrix inverse is returned
##
## Usage:  
## cacheSolve(y) where y is the special vector function previously 
##   assigned.
##

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
