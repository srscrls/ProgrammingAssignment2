## Theses two functions are supposed to be used in combination to cache the inverse 
## of a matrix. The first function creates a special object which the second will 
##calculate the inverse if it is not already in the cache.

## This function creates a list which sets the value of the matrix, gets the value of
## the matrix, sets the inverse of the matrix and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of x. However, if the inverse has already been
## calculated and is stored in the cache, it would get the inverse from the cache
## instead of calculating it again.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
