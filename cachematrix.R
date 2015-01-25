## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix and returns a list containing 
## a set of functions to set & get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  ## the variable inv is intialized to NULL in this envorinment
  inv <- NULL
  ## set function to the set the value of x with a scoping operator <<-
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get / return the data
  get <- function() x
  ## this function sets the value of the inv variable with the supplied inverse
  setInverse <- function(inverse) inv <<- inverse
  ## get the computed / assigned inverse
  getInverse <- function() inv
  ## return the list with all the functions as named parameters
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function works as follows. This takes a matrix, which in this example,-
## made by the function makeCacheMatrix as defined above. This first checks if -
## the inverse for the input matrix is already computed and stored. This does that -
## by calling the getInverse function. If the getInverse function doesn't return null -
## that means the inverse was already computed, and is available in the inv variable.
## On the contrast if the getInverse returns null, then this function gets the matrix -
## and computes the inverse using the solve function, assigning the result to inv variable.
## In the interest of caching this value in order to avoid repeating the computation, -
## this value is stored using setInverse function, before returning the inverse to the
## caller

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
