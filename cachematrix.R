## Those functions cache the inverse of a matrix than compute it repeatedly

## The `makeCacheMatrix` function creates an object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## The function `cacheSolve` computes the inverse of the object returned
## by `makeCacheMatrix` if it has not already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$getmat()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
