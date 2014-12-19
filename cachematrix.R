## A pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invMatr <- NULL
  set <- function(y) {
    x <<- y
    invMatr <<- NULL
  }
  get <- function() x
  setInverted <- function(inverted) invMatr <<- inverted
  getInverted <- function() invMatr
  list(set = set, 
       get = get,
       setInverted = setInverted,
       getInverted = getInverted)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invMatr <- x$getInverted()
  if(!is.null(invMatr)) {
    message("getting inverted matrix from cache")
    return(invMatr)
  }
  matr <- x$get()
  invMatr <- solve(matr, ...)
  x$setInverted(invMatr)
  invMatr  
}
