## Matrix inversion could be a costly computation. Thus these functions compute 
## and cache the inverse of an invertible matrix.
## 
## According to the instructions of programming assignment, the input matrix 
## could be assumed to be invertible. Thus no check for matrix singularity 
## if performed.

## makeCacheMatrix creates a special "matrix" object that could
## cache its inverse. The function is a list of two functions that:
##
##      (1)  set the input matrix
##      (2)  get the input matrix 
##      (3)  set the inverse matrix
##      (4)  get the inverse matrix
##
## Examples: Create a cached matrix object.
##
##      Example 1:
##              newCacheMatrixObject <- makeCacheMatrix()
##              mat <- matrix(rnorm(81),9)
##              newCacheMatrixObject$set(mat)
##
##      Example 2:
##              mat <- matrix(c(1:4),2)
##              newCacheMatrixObject <- makeCacheMatrix(mat)
##

makeCacheMatrix <- function(x = matrix()) {

        ## initialize cached inverse matrix
        c_inv <- NULL 
        
        ## set the input matrix
        set <- function(y) {
                x <<- y
                c_inv <<- NULL
        }

        ## get the input matrix 
        get <- function() x

        ## store the inverse matrix in cache
        setinv <- function(inverse) c_inv <<- inverse

        ## get the inverse matrix from cache
        getinv <- function() c_inv

        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of a makeCacheMatrix object. Following the guidelines 
## of Programming Assignment 2, it could assumed that the input matrix is
## invertible. Thus, in this code, no check for matrix singularity is performed.
##
## The function does the following:     
##      (1) check if the inverse is already on cache
##      (2) if inverse is not on cache, compute the inverse then store in cache
##
## Example: Use of cacheSolve
##
##      inverse <- cacheSolve(newCacheMatrixObject)
##


cacheSolve <- function(x, ...) {

        ## Check if the matrix inverse is cached.
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Using cached inverse matrix.")
                return(inv)
        }
        
        ## Retrieve input matrix from the makeCacheMatrix object.
        inp <- x$get()
        
        ## Calculate for the inverse. Note that R's solve function will return
        ## an error if the input matrix is singular (i.e. not invertible).
        inv <- solve(inp, ...)
        
        ## Store the calculated inverse matrix in cache
        x$setinv(inv)
        
        ## Return inverse matrix
        inv
}
