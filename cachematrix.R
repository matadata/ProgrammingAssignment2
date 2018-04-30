# Matrix inversion is usually a costly computation, 
# therefore caching the computed result can be of great advantage. 
# The following two functions are used to cache, store and retrieve the inverse of a given matrix.


# makeCacheMatrix creates a list, containing a matrix and 4 functions to ...
# 1.  set values of the matrix
# 2.  get values of the matrix
# 3.  set the inverse of the matrix
# 4.  get the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) invm <<- inverse
  getInverse <- function() invm
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Return a matrix for the inverse of 'x' - if not computed yet, store=cache the result, otherwise return from "cache"
#   Assumption: the matrix is always invertible, thereby function solve can be used.
#   
cacheSolve <- function(x, ...) {
  cachem <- x$getInverse()
  if (!is.null(cachem)) {
    message("getting cached data")
    return(cachem)
  }
  m <- x$get()
  invm <- solve(m, ...)
  x$setInverse(invm)
  invm
}

#
# Test function calls
#
set.seed(42)
m <- matrix(rnorm(4, 0, 2), 2)
# first using the function used for computing the inverse matrix
solve(m)

# test using the functions
mc <- makeCacheMatrix(m)
cacheSolve(mc)  #return matrix after computing the inverse matrix
# test again
cacheSolve(mc)  #return matrix from cache
