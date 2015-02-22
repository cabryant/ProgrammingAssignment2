## This pair of functions facilitates caching the inverse of a
## matrix.  Matrix inversion is a costly computation that warrants
## a caching solution when the inverse may have to be computed
## multiple times (e.g. in a loop).

## Creates a wrapper for the passed matrix that supports caching and
## retrieving the inverse of the matrix, as a list containing the
## following functions:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set (i.e. cache) the value of the inverse of the matrix
##  4. get the [cached] value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # re-initializes the matrix and the cached inverse value
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  get <- function() x
  # Note: the <<- operator assigns the result to the variable 'i' in the parent environment
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x', where 'x' is
## a wrapped matrix returned by the makeCacheMatrix function.
## Returns the cached value of inverse of 'x' if it has already 
## been calculated. Otherwise, the inverse is calculated, cached,
## and returned.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retrieving inverse from cache")
    return(i)
  }
  # calculate, cache, and return the inverse of x
  i <- solve(x$get(), ...)
  x$setinverse(i)
  i
}
