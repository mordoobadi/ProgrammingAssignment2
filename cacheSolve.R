cacheSolve <- function(x, ...) {
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