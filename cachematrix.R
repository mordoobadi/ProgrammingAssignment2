#### Put comments here that give an overall description of what your
#### functions do

#### Write a short comment describing this function
## function makeCacheMatrix creates a cached matrix 
## A Cached Matrix stores the original matrix and 
## its inverse. 
##
## FUNCTIONS:
##      $get()    - Returns the original matrix object
##      $set()    - Assigns a matrix object
##      $getinv() - Returns the inverse of the matrix
##      $setinv() - Assigns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#### Write a short comment describing this function

## Solves for inverse of the matrix contained in the
## cached matrix x and returns it.
## If the inverse of this matrix is avaialable it is
## going to be returned otherwise it will be computed 
## and then returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinv()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinv(invrs)
  invrs
}