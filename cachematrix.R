## makeCacheMatrix is a function that creates a special matrix 
## object that can cache its inverse. using CacheSolve, the inverse
## of the matrix will be calculated. When the inverse has already 
## been calculated, the inverse that is stored in the cache will 
## be used, otherwise, the inverse will be calculated

## Create a special matrix object that provides a list of methods
## to set and get the inverse of a matrix and allows to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## declare inverse to null to start out with  
    inverse_x <- NULL
   
    ## set the values passed in
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    
    ## gets the value passed in
    get<- function() x
    
    ## sets the inverse of the matrix
    setInverse <- function(inverse) inverse_x <<- inverse
    
    ## gets the inverse that has already been set
    getInverse <- function() inverse_x
    
    ## list the methods
    list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## The cacheSolve function returns the inverse of the "x" matrix that is 
## passed in. It uses the makeCacheMatrix function to first see if the 
## inverse is already cached, if its not it calculates the inverse and 
## then it stores it for later use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
   
    ## if inverse is not null then it means that the inverse has already been calculated
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return (inverse)
    } 
    
    ## if it gets here then the inverse needs to be calculated
    data <- x$get()
    inverse <- solve(data)
    
    ## set the inverse once it has been calculated
    x$setInverse(inverse)
    
    ## return the inverse
    inverse  
}
