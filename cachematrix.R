## The functions below are used to find and cache the inverse of a matrix

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special matrix but if it has already been calculated it will then be retrieved from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
}

## Test
mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
mymatrix$get()
mymatrix$getinv()
cacheSolve(mymatrix)
mymatrix$getinv()
cacheSolve(mymatrix)
