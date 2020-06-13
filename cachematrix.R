## Inverse calculation of an matrix and caching.

makeCacheMatrix <- function(x = matrix()) {
##Initialize values
  mat.inv <- NULL
  
##Defines the functions for the makeCacheMatrix object
  setmat <- function(y) {
    x <<- y
    mat.inv <<- NULL
  }
  
  getmat <- function() x
  setmatinv <- function(solve) mat.inv <<- solve
  getmatinv <- function() mat.inv
  
##Returns a list
  list(setmat = setmat, getmat = getmat,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## Retrieve cached reverse matrix

cacheSolve <- function(x, ...) {
#Take the matrix
  mat.inv <- x$getmatinv()
  
#Evaluate whether mat.inv is null. If is not null the cached value is returned
  if(!is.null(mat.inv)) {
    message("getting cached data")
    return(mat.inv)
  }
  
#If it is null, calculate the inverse of the matrix
  mat <- x$getmat()
  mat.inv <- solve(mat, ...)
  x$setmatinv(mat.inv)
  mat.inv
}
