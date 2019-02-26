## makeCacheMatrix creates a special matrix object, 
## and then cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) i <<-inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return inverse of matrix x
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated,then the cachesolve retrieves the inverse from the cache.
## param x, is a special matrix created with makeCacheMatrix.
## return, returns the inverse of the matrix x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  else {
    i <- solve(x$get())
    x$setinverse(i)
    return(i)
  }
}
