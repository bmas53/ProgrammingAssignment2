## Overall these functions store the data in cache memory so that R 
## does not have to re-run them every time they are needed
## For example, inverting a matrix can be costly with respect to computing time 
## and computing power.  These functions save on these computing costs.

## This function creates a list.  In this list are functions that do two things:
## First, the function sets the value of the vector
## Second, the function gets the value of the mean 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinv <- function(inverse) m <<- inverse
        getinv <- function()m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function cacheSolve will check to see if the inverse of the matrix has 
## already been calculated.  If it has, then it pulls it from the cache (saving
## computation time); if it hasn't, then it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){message("getting cached data")
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
        }
