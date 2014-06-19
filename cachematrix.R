## This functions create a cache object for calculation of inverse
## of a square matrix and pass the cached object into a function
## that will solve and print the result

## Write a short comment describing this function

## Basic assumption: the matrix must be square matrix
## create a cacheable matrix object and set a function that will get
## cached matrix
makeCacheMatrix <- function(x = matrix()) {
    
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invmat <<- solve
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
## Pass the cached matrix into this function and calculate x the inverse
## of the cached matrix usin get object and return the inverse
## of the cached matrix
cacheSolve <- function(x, ...) {
    invmat <- x$getinv()
    if(!is.null(invmat)) {
        message("getting cached matrix")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data, ...)
    x$setinv(invmat)
    invmat
        ## Return a matrix that is the inverse of 'x'
}
