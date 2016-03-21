## R Programming Course - Assignment 2 - Cached Matrix Inverse
##
## Return the inverse of a matrix and cache it
## If a cached value exists for a given matrix, then use it.
## Otherwise, perform a computation.

## Makes a special "matrix" object that can cache its inverse
## Create a list containing a function to:
##    1 - set the value of the matrix
##    2 - get the value of the matrix
##    3 - set the value of the inverse
##    4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then the inverse is retrieved from the cache rather than computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## First check if inverse is cached 
    m<-x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Perform computation and save it
    data<-x$get()
    m<-solve(data, ...)
    x$setinverse(m)
    m
}
