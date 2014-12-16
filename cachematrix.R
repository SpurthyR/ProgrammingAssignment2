## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it.	
## The below pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		##If the inverse has already been calculated (and the matrix has not changed), 
		##then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  } >
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
