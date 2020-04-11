## This both functions allows to calculate the inverse of a matrix x, to save it in an
#internal function and recover it later. 

## First function creates an object of makeCacheMatrix type, the kind of object that can
#be used with the second fonction. This kind of object contains set and get funcions which
#allows to get an inverse matrix saved in caché (for a matrix x) or to set the matrix
#calculate in second fonction.

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y){
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinv <- function(solve) a <<- solve
    getinv <- function() a
    list (set = set, get = get, setinv = setinv, getinv=getinv)
}


## Second function allows to get an inverse matrix saved before in the makeCacheMatrix
#object or to calculate this matrix, print it, and save it in the set function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    a <- x$getinv()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinv(a)
    a
}