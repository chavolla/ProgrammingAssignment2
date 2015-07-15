##   The following functions are created to avoid the calculation
## of the inverse of a given matrix that has been already calculated.
##   This is done by implementing a function that creates an object 
## that can store the a matrix and its inverse. While the other 
## function checks if the inverse has been already calculated
## in order to calculate the inverse or return the stored one


##   This function creates an object representing a non singular 
## Matrix that is able to store its own inverse matrix.
##   args: x is a non singular matrix 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(m){
        x <<- m
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv){ inverse <<- inv }
    
    getInverse <- function() inverse
    
    list( set = set, get = get,  
          setInverse = setInverse, getInverse = getInverse)
}


##   This funtion checks the a makeCacheMatrix object to verify 
## if the inverse of the given object has been already calculates.
## if the inverse has been already calculated and stored inisde
## the makeCacheMatrix object, it will be returned, otherwise
## the inverse will be calculated and stored in the object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Inverse matrix found in cached")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m,...)
    x$setInverse(inverse)
    inverse
}
