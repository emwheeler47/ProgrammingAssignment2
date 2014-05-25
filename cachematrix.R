## These functions cache a matrix and its inverse then return the inverse value

## The function makeCacheMatrix saves a cache of a matrix and its inverse 
## and defines several functions that allow the user to set the matrix, 
## set the inverse or return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

  cacheInverse <- NULL
  
  setInverse <- function(inv) cacheInverse <<- inv
  getInverse <- function() cacheInverse
  setMatrix <- function(newMatrix) {
      x <<- newMatrix
      cacheInverse <<-NULL   
  }
  getMatrix <- function() x
    
  list(setInverse=setInverse, getInverse=getInverse, set=setMatrix, get=getMatrix)
  
}


## The cacheSolve function returns the cached inverse matrix. If the inverse has not 
## been cached previously, cacheSolve computes and stores the inverse matrix. 


cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if(is.null(inv)) {
      message("Computing inverse matrix...")
      inv <- solve(x$get())
      message("Storing inverse matrix...")
      x$setInverse(inv)
    
  } else {
    
      message("Inverse is being returned from cache")
  }
  
  inv
  
  
}
