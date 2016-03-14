## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function makes the special list that can cache the
## inverse of a matrix. It will be used later on by the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        setmatrix <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The CacheSolve function computes the inverse of the special matrix
## created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinverse()
        if (!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$getmatrix()
        invmat <- solve(data, ...)
        x$setinverse(invmat)
        invmat
}
