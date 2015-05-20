## Matrix invesion is usually a costly computation and there may be benefit  to 
## caching the inverse of a matrix rather than computing it repeatedly.
## These functions cache the inverse of a matrix in order to minimize
## computation time.

## makeCacheMatrix creates a special "matrix" which is really a list containing
## a function to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse of the matrix and get the value of the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already
## been calculated. If so, it 'get's the mean from the cache and skips the
## computation. Otherwise, it cacluates the inverse of the data and sets the
## value of the inverse in the cache via the 'setinv' function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)  
  x$setinv(inv)
  
  inv
}
