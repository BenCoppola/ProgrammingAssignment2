## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The purpose of the following functions is to provide a mechanism to cache
## a previously computed matrix inversion.


## The makeCacheMatrix function takes an argument 'x' that is a matrix.
## It creates a list of functions which can do the following:
## 1. Set: stores a matrix 
## 2. Get: retrieves a matrix
## 3. SetInverse: computes and stores the inverse of a matrix
## 4. GetInverse: retrieves the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## Define the first function, set, which will store a new matrix 
        ## in the list created by the makeCacheMatrix function
        set <- function(y) {
                x   <<- y
                inv <<- NULL    # 
        }
        ## Define the second function, get, which will retrieve the matrix
        ## stored in the list created by the makeCacheMatrix function
        get <- function() x
        
        ## Define the third function, setInverse, which will compute the 
        ## inverse of the matrix using the built-in R function 'solve'
        ## (NOTE: Matrix must be invertible)
        setInverse <- function(){
                inv <<- solve(x)
        }
        ## Define the fourth function, getInverse, which will retrieve the 
        ## pre-computed inverse of the matrix.  This function will return 
        ## NULL if the setInverse command has not yet been executed
        getInverse <- function() inv
        
        #return the four functions to the list as output
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function takes an argument 'x' that is a list as produced
## by the makeCacheMatrix function. This function first tries to retrieve 
## the cached inverse of the matrix that is already stored in the list. 
## If the cached inverse is not available, it will be calculated and 
## subsequently stored in the list for future use.

cacheSolve <- function(x, ...) {
        ## First, try to retrieve the cached data
        inv <- x$getInverse()  
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the code reaches here, that means the inverse wasn't already cached
        message("computing and storing in cache")
        x$setInverse()        # compute and cache the inverse for later use
        x$getInverse()
}
