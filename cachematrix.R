## In order to save computation time, makeCacheMatrix creates a special 'matrix' from which cacheSolve can 
## either calculate its inverse or access the inverse from the cache if it has already been computed

## Creates a special 'matrix' which is actually a list containing functions to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse, and get the value of the inverse
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


## First checks whether the inverse has been calculated already in which case the function gets the 
## inverse from the cache, otherwise calculates and returns the inverse
cacheSolve <- function(x, ...) {
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
