## A pair of functions, makeCacheMatrix() and cacheSolve(), are written 
## to cache the inverse of a matrix. This one alternative to matrix inversion
## would save from repeated computation.

## makeCacheMatrix creates a special 'matrix' object that can cache
## its inverse. makeCacheMatrix is actually a list containing
## a function to set the values of the matrix, get the values of the matrix,
## set the values of the inverse of the matrix (setinv), and
## get the values of the inverse of the matrix (getinv).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The function cachesolve computes the inverse of the special 'matrix'
## returned by makeCacheMatrix. It first checks to see if the inverse has
#  already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the mean of the data and sets the value of the inverse in the cache 
## via the setinv() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  data <- x$get()
        ## solve() function computes the inverse of a square matrix.
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}