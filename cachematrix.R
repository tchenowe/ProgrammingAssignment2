
# makeCacheMatrix returns an object with methods for setting and returning a matrix
# and storing and returning the inverse of the Matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# A function that accepts a makeCacheMatrix object, checks to see if a matrix's 
# inverse is cached in the object and if it is returns the inverse. If the inverse 
# is not cached it retrieves the matrix stored in the object, computes the inverse
# of the matrix, caches the value of the inverse in the object and then returns 
# the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv    
}
