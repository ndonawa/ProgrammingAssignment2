## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-  function(y) {
        x <<- y
        inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inverse <<- solve(x)
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)){
      message("returning cached data")
      return(inverse)
  }
  solutions <- x$get()
  inverse <- mean(solutions, ...)
  x$setinv(inverse)
}
