## makeCacheMatrix() creates matrix "objects" that "herits" the functions
## made into the makeCacheMatrix() function. The cacheSolve() function uses
## the local variables created into the created object to calculate the inverted
## matrix into that object and cache the result into it, to read it later
## instead of computing it again.

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object than herits the functions
## given on the list (set, get, setCacheMatrix, getCacheMatrix)
## set : to set the object values
## get : to get the object values
## setCacheMatrix : to set in cache the inversed matrix of the matrix object
## getCacheMatrix : to get the cached inversed matrix of the matrix object

makeCacheMatrix <- function(x = matrix()) {
    # cached inversed matrix init
    inverse <- NULL 
    # set the matrix 
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    # get the Matrix 
    get <- function() x  
    # set the cached Matrix
    setCacheMatrix <- function(solve) inverse <<- solve 
    # get the cached Matrix
    getCacheMatrix <- function() inverse 
    # list of the functions
    list(set=set, get=get, setCacheMatrix=setCacheMatrix, 
         getCacheMatrix=getCacheMatrix)
}


## cacheSolve() solves the matrix object created with the makeCacheMatrix()
## function. It first test if there already is a cached inverted matrix of the
## object (getCacheMatrix() from the makeCacheMatrix() function) instead of
## recalculate it. If there isn't, then the inverted matrix is written 
## with setCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getCacheMatrix()
        if(!is.null(inverse)) {
            message("Getting cached inverted matrix")
            return(inverse)
        }
        inputmatrix <- x$get()
        inverse <- solve(inputmatrix, ...) 
        x$setCacheMatrix(inverse)
        inverse
}