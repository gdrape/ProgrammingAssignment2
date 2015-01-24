## This R script caches the inverse of a matrix, which allows
## potentially intensive computations to be done in a separate cache
## environment (name space in R).

## The function makeCacheMatrix enables a matrix to be stored and 
## retrieved from cache.
## The input is a numeric matrix (assumed to be square and invertible).
## The output is a list of functions to set (store) the matrix 
## name and set its value in cache, as well as get (retrieve) the
## matrix name and get its value from cache.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL            
                # Initialize name 'mat' to hold cached matrix
        
        set <- function(y) {   
                # Resets original matrix and clears inverse from cache
                x <<- y        
                        # Assigns the input argument 'y' to object
                        # 'x' and caches its value in a new 
                        # environment created by 'set' function
                mat <<- NULL   
                        # Assigns null value to the object 'mat' 
                        # in effect clearing its value
                        # and stores it in the cached environment
        }       # End function 'set'
        
        get <- function() x                          
                # Returns value of the variable 'x' from cache

        setmatrix <- function(matrix) mat <<- matrix 
                # Takes the input argument 'matrix' and sets its
                # value to the name 'mat'
        
        getmatrix <- function() mat                  
                # Function to return name 'mat', which points
                # to the matrix in the cache
        
        list(set = set, get = get,    
             setmatrix = setmatrix,
             getmatrix = getmatrix)
                # Return list with pointers to the four functions
        
}   # End function 'makeCacheMatrix'


## This function finds the inverse of the matrix stored in cache
## The input is the functions created in 'makeCacheMatrix'.
## The function returns the inverse of the matrix input to 
## 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {   
        
        mat <- x$getmatrix()       
                # Executes 'getmatrix' function from list 'x', 
                # which returns the name 'mat' that points to
                # the matrix
        
        if(!is.null(mat)) {        
                # If 'mat' (container for the inverse) is not null  
                message("getting cached data") 
                        # Prints message that cached data is retrieved
                return(mat)        
                        # Returns matrix object 'mat'
        } # End if                         
                
        data <- x$get()            
                # Executes 'get' function from list 'x', 
                # which returns a value from cache
                # and assigns its value to name 'data'
        
        mat <- solve(data, ...)    
                # Finds the inverse of the data, assigns to matrix 
                # 'mat'. This accomplished using function 'solve' 
                # with single argument. 
        
        x$setmatrix(mat)           
                # Executes 'setmatrix' function from list 'x'
                # which stores the inverse in cache.
        
        mat                        
                # Returns a matrix 'mat' that is the inverse of the 
                # original matrix 'x' from 'makeCacheMatrix'.

}   # End function 'cacheSolve'
