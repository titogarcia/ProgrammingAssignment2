## Functions in this file are designed to cache the time-consuming
## computation of calculating the inverse of a matrix.

## Creates a special "matrix", which is really a list containing functions:
## - set: set the value of the matrix
## - get: get the value of the matrix
## - setinverse: set the cached inverse matrix
## - getinverse: get the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()
           x
  
  setinverse <- function(inverse)
                  inv <<- inverse
  
  getinverse <- function()
                  inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a special "matrix", using the
## cached inverse if available.
cacheSolve <- function(x, ...) {
  res <- x$getinverse()
  
  if (!is.null(res)) {
    message("getting cached inverse")
    return(res)
  }
  
  res <- solve(x$get(), ...)
  x$setinverse(res)
  
  res
}
