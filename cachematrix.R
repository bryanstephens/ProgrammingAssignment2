## This is a pair of functions that cache the inverse of a given matrix. These should be called in
## a fashion such as:
## ```
## c=rbind(c(1, -1/4), c(-1/4, 1))
## cachingMatrix <- makeCacheMatrix(c)
## cacheSolve(cachingMatrix)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## ```

## This creates an object capable of storing a matrix and its inverse.
## In order to calculate and store the inverse one should call cacheSolve() on an instance
## created from this method in order to cache the value and save computations.
makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse as NULL when creating new instance.
  inverse <- NULL
  
  ## Set current matrix as newMatrix and reset cached inverse.
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  ## Get the current stored matrix.
  get <- function() x
  
  ## Used by cacheSolve() function to set the cached inverse value.
  setInverse <- function(newInverse) inverse <<- newInverse
  
  ## Get the current calculated inverse, this can return null if cacheSolve() has not been called.
  ## Seems like one would like to call cacheSolve with reference to this instance that way 
  ## htey don't need to call a separate method to get the value off of this object. But alas
  ## search for 'this' as a concept in R has yielded no results and it seems like the design
  ## of this assignment is opposite from how I would like to implement this personally.
  getInverse <- function() inverse
  
  ##Registers the different functions of this object.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This uses an instance created from makeCacheMatrix() as a caching store for the calculated
## inverse. This relies on the object created from makeCacheMatrix() to reset the cached inverse
## to function properly.
cacheSolve <- function(x, ...) {
  ## Get the value from the 'special' caching matrix.
  inverse <- x$getInverse()
  
  ## If the caching matrix has no current stored value for the inverse then calculate it and store it.
  if(is.null(inverse)) {
    inverse <- solve(x$get())
    x$setInverse(inverse)
  }
  
  ## Contains either the cached or newly calculated value, return it.
  inverse
}
