
## These functions provide the ability to cache the inverse of a matrix rather than 
## having to compute it repeatedly.  These functions can be leveraged in a loop
## or other functions where the inverse of a matrix would be reused.
##
## Funtions:
## makeCacheMatrix: Creates a special "matrix" that stores the matrix inversion
## cacheSolve: Calculates and returns a matrix that is the inverse of the special matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix", which is really a list containing functions to:
  #
  # set the value of the matrix
  # get the value of the matrix
  # set the value of the matrix inversion
  # get the value of the matrix inversion
  #
  # Args:
  #   x: The matrix to be converted to a special "matrix"
  #
  # Returns:
  #   a list containing "getters" and "setters" for the matrix and its inversion
  
  cached.inversion <- NULL
  
  setMatrix <- function(square.matrix) {
      x <<- square.matrix
      cached.inversion <<- NULL
  }
  
  getMatrix <- function() {
      x
  }
  
  setMatrixInversion <- function(inversion) {
      cached.inversion <<- inversion
  }
  
  getMatrixInversion <- function() {
      cached.inversion
  }
  
  list(set.matrix = setMatrix, get.matrix = getMatrix,
       set.inversion = setMatrixInversion,
       get.inversion = getMatrixInversion)
}


cacheSolve <- function(x, ...) {
  # Calculates and returns a matrix that is the inverse of the special matrix 'x'
  #
  # Args:
  #   x: The special matrix to be inverted
  #
  # Returns:
  #   a matrix that's the inverse of x
 
  inversion <- x$get.inversion()
  
  if (!is.null(inversion)) {
      message("getting cached data")
      return(inversion)
  }
  
  data <- x$get.matrix()
  inversion <- solve(data, ...)
  x$set.inversion(inversion)
  inversion
}
