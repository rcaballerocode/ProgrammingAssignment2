## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  mtx_inverse <- NULL
  
  set <- function(matrix_obj) {
    m <<- matrix_obj
    mtx_inverse <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse_cal) {
    mtx_inverse <<- inverse_cal
  }
  
  getInverse <- function() {
    mtx_inverse
  }
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mtx_inverse <- x$getInverse()
  
  if (!is.null(mtx_inverse)) {
    message("getting cached data")
    return(mtx_inverse)
  }
  
  matrix <- x$get()
  
  inverse_cal <- solve(matrix) %*% matrix
  
  x$setInverse(inverse_cal)
  
  inverse_cal
}
