## These functions make code that has to repeatedly take the inverse of a
## matrix faster by allowing the program to only calculate the inverse once and
## recall it later, rather than calculating the inverse over and over again.


## This function creates a list of four functions: set, get, set.inv, and 
## get.inv. It also memorizes the values for m and x.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set.inv <- function(inv) i <<- inv
        get.inv <- function() i
        list(set = set, get = get,
             set.inv = set.inv,
             get.inv = get.inv)
        
}


## This function takes the inverse if it hasn't already been calculated.
## If it has, this function returns the cached inverse value.

cacheSolve <- function(x, ...) {
        i <- x$get.inv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inv(i)
        i
}