## This functions makes an special Matrix, with 4 functions
## the functions works for caching the inverse matrix
## result, the makeCacheMatrix returns a List with the 4
## functions, and the cacheSolve does the Matrix Inverse 
## computation


## makeCacheMatrix returns a list with 4 functions
## to manipulate the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    matrixInverse <<- inverse
  }
  
  getinverse <- function() matrixInverse
  
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns an inverse of special matrix of x
## it must be one returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  matrixInverse <- x$getinverse()
  if (!is.null(matrixInverse)) {
    message("getting cache data")
    return (matrixInverse)
  }
  
  data <- x$get()
  
  matrixInverse <- solve(data, diag(nrow = 2, ncol = 2))
  
  x$setinverse(matrixInverse)
  
  matrixInverse
}
