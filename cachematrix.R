## This pair of functions calculates, stores, and retrieves the inverse of a square matrix.
## It saves computational power and time and frees up memory.

## This function creates a list of functions that set and get the original matrix as well
## functions that set and get the inverse of that original matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setm <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getm <- function() x
  setinv <- function(inverse) {
    inv <<- inverse
  }
  getinv <- function() inv
  list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}

## This function checks to see if the inverse of the matrix has already been stored.
## If so, it returns a message along with the cached inverse of the matrix. If not,
## it calculates the inverse, stores it, and then returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
          message("Getting cached matrix inverse")
          return(inv)
    }
    data <- x$getm()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  ## Return a matrix that is the inverse of 'x'
}
