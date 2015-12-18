## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
        # get the value of the inverse
        getinverse <- function() inv
        # return the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # get the result of the inverse
        inv <- x$getinverse()
        # check if the result has already been calculated
        # if yes, return the inverse directly
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # if not, use the functions from makeCacheMatrix to 
        # to calculate the inverse through solve() 
        data <- x$get()
        inv <- solve(data, ...)
        # and to set the inverse through setinverse()
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}