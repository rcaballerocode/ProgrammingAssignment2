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
  
  ## Just return the inverse if its already set
  if (!is.null(mtx_inverse)) {
    message("getting cached data")
    return(mtx_inverse)
  }
  
  ## Get the matrix
  matrix <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverse_cal <- solve(matrix) %*% matrix
  
  ## Set the inverse of the matrix
  x$setInverse(inverse_cal)
  
  inverse_cal
}
