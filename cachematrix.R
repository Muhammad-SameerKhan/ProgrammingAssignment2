# 1.makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# The function returns a list containing methods to set the matrix, get the matrix,
# set the cached inverse, and get the cached inverse
# 2.The cacheSolve function calculates the inverse of a matrix. It takes a special "matrix"
# object x as input, which should have been created using the makeCacheMatrix function. 
# The purpose of cacheSolve is to efficiently compute the inverse while utilizing caching 
# to avoid redundant computations.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    cached_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cached_inverse <<- mean
  getInverse <- function() cached_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cached_inverse <- x$getInverse()
  if(!is.null(cached_inverse)) {
    message("getting cached inverse")
    return(cached_inverse)
  }
  matrix_data <- x$get()
  Inverse <- solve(matrix_data, ...)
  x$setInverse(inverse)
  return(inverse)
}
