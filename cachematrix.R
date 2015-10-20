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
    # Initialize the cached inverted matrix
    inverse <- NULL 
    # Set the object's matrix
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    # Get the object's matrix
    get <- function() x  
    # Set the inverted matrix into cache. Used with cacheSolve() only.
    setCacheMatrix <- function(solve) inverse <<- solve 
    # Get the inverted matrix from the cache.
    getCacheMatrix <- function() inverse 
    # List of the functions included into makeCacheMatrix() objects
    list(set=set, get=get, setCacheMatrix=setCacheMatrix, 
         getCacheMatrix=getCacheMatrix)
}


## cacheSolve() solves the matrix object created with the makeCacheMatrix()
## function. It first test if there already is a cached inverted matrix of the
## object (getCacheMatrix() from the makeCacheMatrix() function) instead of
## recalculate it. If there isn't, then the inverted matrix is written 
## with setCacheMatrix().

cacheSolve <- function(x, ...) {
        # Check if there is a cached inverted matrix
        inverse <- x$getCacheMatrix()
        if(!is.null(inverse)) {
            # If there is, then print a message and return it
            message("Getting cached inverted matrix")
            return(inverse)
        }
        # If there is not, get the original matrix, inverse it, set it into
        # cache and return the inverted matrix.
        inputmatrix <- x$get()
        inverse <- solve(inputmatrix, ...) 
        x$setCacheMatrix(inverse)
        inverse
}