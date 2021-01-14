## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions will cache inverse of a matrix.

## following function makes special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}

## following function computes inverse of special matrix created by above function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
        
}

## matrix that is inverse of x
##pmatrix$getInverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
