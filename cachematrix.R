## These functions will calculate the inverse of a matrix and
## store the result preventing the recalculation of a given matrix.

## makeCacheMatrix sets up a vector of functions which can
## store a matrix and store information about it in a retrieviable
## format.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)   
}


## cacheSolve checks the matrix made by makeCacheMatrix
## and either returns the inverse of 'x' if it has already
## been calculated or triggers the calculaton and returns
## the result.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    ## If the inverse of x has already been calculated
    ## return the result.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If the inverse of x hasn't been calculated, 
    ## caclulate it and store & return the result.
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
