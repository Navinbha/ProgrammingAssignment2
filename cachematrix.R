## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
