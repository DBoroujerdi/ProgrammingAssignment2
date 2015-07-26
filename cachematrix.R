## The following functions allow for caching of matrix inverse.
## This allows for the bypassing of matrix solving via solve() 
## which can be costly for large matrices. They acheive this via the
## makeCacheMatrix which takes advantage of R scoping to store 
## caches values in the object's scope and the cacheSolve function
## wraps the solve() function, only calling it when the result is not
## already stored in the matrix cache.


## Returns a new R object, storing an underlying matrix
## and allows for the caching of the inverse of that matrix.
## Functions: 
##    setmatrix   - set the matrix in the objects scope
##    getmatrix   - get the matrix from the objects scope
##    setinverse  - set the inverse in the objects scope
##    getinverse  - get the inverse in the objects scope
##
## Args:
##    x - A matrix
makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  
  setmatrix <- function(matrix) {
    x <<- matrix
    cachedinverse <<- NULL
  }
  
  getmatrix <- function() x
  
  setinverse <- function(inverse) cachedinverse <<- inverse
  getinverse <- function() cachedinverse
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## Solves the inverse of the matrix x, where x is a special cacheable
## matrix returned by the function makeCacheMatrix. If the cacheable matrix
## contains the inverse, that is returned, bypassing the costly solve() step, otherwise,
## the inverse of the matrix is solved and stored on the cacheable matrix
##
## Args:
##    x     - Cacheable matrix - created with makeCacheMatrix
##    ...   - Additional arguments to be passed to underlying solve()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("returning cached")
    
    return(inverse)
  }
  
  m <- x$getmatrix()
  inverse <- solve(m, ...)
  x$setinverse(inverse)
  inverse
}
