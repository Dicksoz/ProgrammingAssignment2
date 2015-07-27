## These functions allow the creation of a matrix object which can have its
## inverse cached to prevent costly recalculation on subsequent calls

## Name: makeCacheMatrix
## Usage: cacheMatrix <- makeCacheMatrix(x)
## ARGS: x - A matrix to be cached
## Description: Creates and returns a list of functions for setting and
##    recalling the values of the matrix and its inverse. The inverse is
##    initially set to NULL. The matrix and its inverse are stored in the
##    appropriate parent frame
## Warnings: A warning will be generated if x is not a square matrix
## Return Value : A list of functions(set, get, setInverse, getInverse)
makeCacheMatrix <- function(x = matrix()) {
      if(ncol(x) != nrow(x)) {
            warning("The inverse of a matrix can only be calculated for a",
                  " square matrix\n\tSubsequent Calls to cacheSolve will",
                  " fail")
      }
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- solve(y)
      }
      get <- function() x
      setInverse <- function() I <<- solve(X)
      getInverse <- function() I
      list(set = set, get = get, setInverse = setInverse, 
           getInverse = getInverse)
}


## Name: cacheSolve
## Usage: InverseMatrix <- cacheSolve(x, ...)
## ARGS:    x - A cacheMatrix returned by makeChacheMatrix
##          ... - A list of additional arguments to be passed to the internal
##                solve function
## Description: Calculates and returns the inverse of the matrix contained
##    within x. Saves time when multiple calls to inverse will be made by first
##    checking to see if the inverse has already been calculated, and then
##    calculating and caching the inverse
## Messages : A message will be displayed if the inverse is being retrieved
##    from the cache rather than being calculating
## Return Value : A matrix which is the inverse of the data of x
cacheSolve <- function(x, ...) {
      I <- x$getInverse()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      x$setInverse()
      I <- x$getInverse()
      I
}
