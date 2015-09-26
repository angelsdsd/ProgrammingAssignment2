## Put comments here that give an overall description of what your
## functions do
## Caching the Inverse of a Matrix:
## Matrix inversion is normally a costly computation hence caching an inversion
## of an existing matrix can be beneficial rather than calculating the inversion everytime.
## Below are couple of functions to stores a matrix and its inversion / calculates its 
## inversion.

## Write a short comment describing this function
## This function creates a special Matrix object that store the Matrix itself as well as
## cache its inversion.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Function to reset the Matrix to a new one.
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        ## Function to get / display the current Matrix.
        get <- function() x
        
        ## Function to set the inverse of a given Matrix (will be call in 2nd Func)
        setInverse <- function(inverse) inv <<- inverse
        ## Function to get the inverse of a given Matirix
        getInverse <- function() inv
        
        ## Store all 4 function in a list can be called by makeCacheMatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## This function calculate the inversion of the Matrix set in 1st function.
## 1. If the inversion has been calculated already (Matrix hasn't been changed), then
##    it will be retrived from the cache.
## 2. Otherwise, it will calculate the inversion for the newly provided Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  ## Tried to retrieve the inversion from cache
        if (!is.null(inv)) {
                message("Matrix exists, getting cached inversion.")
                return(inv)
        }
        ## Else
        new_matrix <- x$get()           ## set newly proved Matrix to new_matrix
        inv <- solve(new_matrix, ...)   ## calculate inversion
        x$setInverse(inv)               ## set new inversion
        inv                             ## return new inversion
}

## Function Ends.
