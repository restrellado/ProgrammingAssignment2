## Put comments here that give an overall description of what your
## functions do

## This function sets the value of a matrix, gets the value of the matrix, 
## sets the value of the matrix's inverse,
## and gets the value of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function returns the cached value of the inverse matrix from
## makeCacheMatrix if it's argument is the same as the originally
## cached matrix. If it is not the same, it sets the inverse of
## the argument.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}