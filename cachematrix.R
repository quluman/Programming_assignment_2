
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(x) {
        m <<- x;
        inverse <<- NULL;
    }
    get <- function() return(m);
    set_inv <- function(inv) inverse <<- inv;
    get_inv <- function() return(inverse);
    return(list(set = set, get = get, set_inv = set_inv, get_inv = get_inv))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(m) {
    inverse <- m$get_inv()
    if(!is.null(inverse)) {
        message("Getting cached data ing")
        return(inverse)
    }
    data <- m$get()
    inverse <- solve(data)
    m$set_inv(inverse)
    return(inverse)
}
