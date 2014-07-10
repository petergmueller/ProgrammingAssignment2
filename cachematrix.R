## caching the inverse of a matrix

## provide a vector containing a list of functions 
## set the matrix
## get the matrix 
## set the inversed matrix 
## get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inversion) invmatrix <<- inversion
  getinverse <- function() invmatrix
  list(setMatrix = set, 
       getMatrix = get,
       setInverse = setinverse,
       getInverse = getinverse)
}


## returns the inverse of a matrix. 
## either from the cache or if not yet in the cache the inversion is done and stored in the cache

cacheSolve <- function(x, ...) {
  invmatrix <- x$getInverse()
  if(!is.null(invmatrix)) {
    #message("getting cached data")
    return(invmatrix)
  }
  data <- x$getMatrix()
  invmatrix <- solve(data, ...)
  x$setInverse(invmatrix)
  message("stored in cached")
  invmatrix
}
