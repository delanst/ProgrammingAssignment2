# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # sets the matrix and the inv to null (meaning cache is empty)
    x <<- y
    inv <<- NULL
  }
  get <- function() x # returns the matrix x
  setInverse <- function(inverse) inv <<- inverse # cache the inverse within the inv variable in another environment.
  getInverse <- function() inv # retreive cached value (inv)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The cacheSolve function returns the inverse of the matrix. A check in the cache is
# performed first and if present then the cache value is returned (so no computation).
# Otherwise it calculates the inverse and places in the cache.
# Placing in the cache happens by x$setInverse
# retrieval from the cache happens by x$getInverse
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # retrieve cached value by returning inv in another environment.
  if(!is.null(inv)) { # check if cached inverse is present if so return cached value
    message("getting data from cache.")
    return(inv)
  }
  data <- x$get() # if not cached get the x value 
  inv <- solve(data, ...) 
  x$setInverse(inv) # sets the value in the cache.
  inv # returns the matrix
}
