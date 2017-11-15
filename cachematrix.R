## Solve inverse of a matrix and cache inverse calculation for reuse.

## This function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  
    set <- function(y) {  
        x <<- y  
        inverse <<- NULL  
    }  
   
    get <- function() x  
    setinverse <- function(inverse) inverse <<- inverse  
    getinverse <- function() inverse  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## Check if an inverse already exists in cache, if the result is null calculate the matrix inverse with solve() 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse() 
    
    if(!is.null(inverse)) {  
        message("getting cached result")  
        return(inverse)  
    }  
   
    data <- x$get()  
    inverse <- solve(data, ...)  
    x$setinverse(inverse)  
    inverse  
}
