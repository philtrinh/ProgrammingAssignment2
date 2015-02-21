## Part of the R language class.
## This module includes two functions:
##  makeCacheMatrix:   Represents a matrix with API to access members of the matrix
##  cacheSolve:        Return the inverse of the matrix provided in the function above.

###############################################################################
#  Function name:  makeCacheMatrix
#  Abstract:       represents a matrix with APIs to access to its members
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
        #Init the inverseX to be null initially
        inv_X <- NULL

        #this set function set the global x value with the given matrix
        set <- function(y){
            x     <<- y
            inv_X <<- NULL
        }

        #this get() function return the current x matrix
        get <- function() x

        #these two helper functions return/set the inverse of x
        getinv <- function() inv_X
        setinv <- function(invX) inv_X <<- invX

        #return the list of available APIs
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)

}


###############################################################################
#  Function name:  cacheSolve
#  Abstract:       Calculate and return the inverse of matrix x.
#                  A check will be performed if the inverse value has
#                  already been calculated.  If so, this cached value
#                  shall be used.
###############################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_X <-x$getinv()

        #return the cached value
        if(is.matrix(inv_X)){
            message("getting cached data")
            return (inv_X)
        }

        #if cached value is not available, then calculate and return
        tempData <- x$get()
        #calculate inverse of X
        inv_X <- solve(tempData)

        #Set inverse data to the main function
        x$setinv(inv_X)

        #return inv_X
        inv_X

}
