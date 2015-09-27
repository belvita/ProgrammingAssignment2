## The purpose of the following pair of functions is to create a square invertible matrix
## and make the inverse of the matrix available in the cache.
###########################################################################

## makeCacheMatrix creates and returns a list of functions
## used by the subsequent function cacheSolve to get or set the inverted matrix in cache.
makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  
  set = function(y) {
      x <<- y
      inv <<- NULL
  }
  
  get = function() x

  setInverse = function(inverse) inv <<- inverse

  getInverse = function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix.
## It first checks to see if the inverted matrix is not in the cache. If yes it
## returns the inverse from the cache. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache .
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
 
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  
  inv <- solve(data, ...)

  x$setInverse(inv)
  inv
}
