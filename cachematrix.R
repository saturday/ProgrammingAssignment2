## This script contains two functions that enable the caching of an inverse matrix
## and returns its value if one exists, otherwise it returns the uncached inverse 
## matrix and caches it.

## Create the cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {

  setMatrix <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(x) s <<- solve(x)
    
  getInverse <- function() s
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Solve for an inverse matrix and retrieved the cached version if one exists.
## Otherwise cache the value of the newly solved inverse matrix and return its
## value.
cacheSolve <- function(x, ...) {
  s <- x$getInverse()

  ## Check if the inverse has already been cached.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## Otherwise find the inverse, cache it and return its value.
  data <- x$getMatrix()
  
  inverse <- solve(data)
  
  x$setInverse(inverse)
  
  inverse
}
