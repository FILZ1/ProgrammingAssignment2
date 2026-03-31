## cachematrix.R
## Functions to cache the inverse of a matrix to avoid redundant computation.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions:
##   set(y)     - store a new matrix (and clear the cached inverse)
##   get()      - retrieve the stored matrix
##   setInverse(inverse) - store a computed inverse in cache
##   getInverse()        - retrieve the cached inverse (NULL if not yet computed)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # cached inverse, starts empty

  set <- function(y) {
    x   <<- y     # update the stored matrix in the parent environment
    inv <<- NULL  # invalidate the cache whenever the matrix changes
  }

  get <- function() x

  setInverse <- function(inverse) inv <<- inverse

  getInverse <- function() inv

  list(set        = set,
       get        = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: Computes (or retrieves from cache) the inverse of the special
## matrix created by makeCacheMatrix.
## - If a cached inverse exists, it is returned immediately with a message.
## - Otherwise the inverse is computed with solve(), stored via setInverse(),
##   and then returned.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)          # return early — no computation needed
  }

  mat <- x$get()         # retrieve the underlying matrix
  inv <- solve(mat, ...) # compute the inverse
  x$setInverse(inv)      # store it in the cache
  inv                    # return the computed inverse
}
