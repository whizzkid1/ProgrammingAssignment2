## In this assignment I have written code for a
## pair of functions that cache the inverse of a matrix.  

## This function will create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(solve_matrix) inv <<-solve_matrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by above function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data)
  x$setInverse(inv)
  inv       ## This will return a matrix that is the inverse of 'x' matrix
}
