## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly
## Below two functions are used to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_matrix <<- solve
  getinverse <- function() inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data for inverse of matrix")
    return(inv_matrix)
  }
  org_matrix <- x$get()
  inv_matrix <- solve(org_matrix, ...)
  x$setinverse(inv_matrix)
  
  ## Return a matrix that is the inverse of 'x'
  inv_matrix
}
