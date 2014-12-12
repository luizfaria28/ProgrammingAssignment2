

## makeCacheMatrix() returns a structure able to mantain a matrix and
## its inverse
## This function returns a list which elements are the following
## functions:
### set: to set the matrix
### get: to get the matrix
### setinv: to set the inverse of the matrix
### getinv: to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <- NULL
    }
    get <- function() x
    setinv <- function(minv) inv <<- minv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() receives a structure returned by makeCacheMatrix()
## representing a matrix and return the inverse of the matrix;

## The inverse matrix is calculated only once; After being calculated,
## the function retrieves the inverse of the matrix previously
## cached on the structure received by parameter.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached inverse of matrix")
            return(inv)
        }
        message("first time: calculate and cache inverse matrix")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
