## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## X is initialed as argument for makeCacheMatrix with default value as an empty matrix
##Inverse is initialized as object within makeCacheMatrix.
## Set takes y (argument assumed to be empty matrix) and assigns values from parent enviornment
## Values will be from matrix
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
##Cache solve will retrieve inverse from object in makeCacheMatrix
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
