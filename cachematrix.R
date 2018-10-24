## This pair of functions cache the inverse of a (squared) matrix; the first 
## creates a special matrix object, and the second one computes its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setcm <- function(solve) cm <<- solve
  getcm <- function() cm
  list(set = set, get = get,
       setcm = setcm,
       getcm = getcm)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and unchanged), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  cm <- x$getcm()
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  data <- x$get()
  cm <- solve(data, ...)
  x$setcm(cm)
  cm
}
