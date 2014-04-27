## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## A pair of functions that cache the inverse of a matrix.
## Written by shabgrd@github


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  cachedInverse <- NULL
  
  set <- function(y) {
    m <<- y
    cachedInverse <<- NULL
  }
  get <- function() m
  
  setInverse <- function(i) cachedInverse <<- i
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  i <- m$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setInverse(i)
  i
}
