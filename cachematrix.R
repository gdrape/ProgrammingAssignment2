## This program caches the inverse of a matrix, which allows
## potentially intensive computations to be done in a separate
## environment.

## The function makeCacheMatrix enables a matrix to be stored and 
## retrieved from cache.
## The input is a matrix object (assumed to be square and invertible).
## The output is a list of functions to set a matrix (the object and
## its value) in cache, and get a matrix (the object and its value)
## from cache.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL            # Initialize object to hold cached matrix
        
        set <- function(y) {   # Function to set value of matrix
                x <<- y        # Assigns y to x in diff environment
                mat <<- NULL   # Assigns null value to output matrix
        }                      # End function 'set'
        
        get <- function() x                          # Function to get value of matrix
        setmatrix <- function(matrix) mat <<- matrix # Function to set matrix object
        getmatrix <- function() mat                  # Function to get matrix object               
        list(set = set, get = get,                   # Return list of four functions
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}   # End function 'makeCacheMatrix'


## This function finds the inverse of the matrix stored in cache

cacheSolve <- function(x, ...) {   # Function to find the inverse of cached matrix
        mat <- x$getmatrix()       # Execute 'getmatrix' function from list 'x', assign to 'mat'
        if(!is.null(mat)) {        # If 'mat' is not null  
                message("getting cached data") # Get the matrix from cache
                return(mat)        # Return matrix object 'mat'
        }                          # End if
        data <- x$get()            # Execute 'get' function from list 'x', assign value to 'data' 
        mat <- solve(data, ...)    # Find the inverse of the data, assign to matrix 'mat'
        x$setmatrix(mat)           # Execute 'setmatrix' function from list 'x'
        mat                        # Return a matrix 'mat' that is the inverse of the original matrix 'x'

}   # End function 'cacheSolve'
