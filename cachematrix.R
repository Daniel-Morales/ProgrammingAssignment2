
# makeCacheMatrix creates a list containing a function that sets the value of the matrix, gets the value of the matrix, 
# sets the value of inverse of the matrix and gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The function returns the inverse of the matrix. If the inverse has already been computed, it 
# I skips the omputation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. Invertibility is assumed.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
