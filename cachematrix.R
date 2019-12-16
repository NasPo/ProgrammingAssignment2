## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.


## makeCacheMatrix builds a set of functions and returns the functions within a list
## to the parent environment.

## two objects are initialized, x and m
## m is set to NULL, initializing it as an object within the makeCacheMatrix() environment
## to be used by later code in the function

## set() setter for x
## get() getter for x
## setsolve() setter for m
## getsolve getter for m

## makeCacheMatrix() assigns each of these functions as an element within a list(),
## and returns a fully formed object of type makeCacheMatrix() to the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve() is required to populate and/or retrieve the inverse from an object
## of type makeCacheMatrix()

## the function attempts to retrieve an inverse from the object passed in as the argument.

## it calls the getsolve() function on the input object
## if the value is not equal to NULL, the cached inverse is returned to the parent environment
## otherwise the inverse of the input object is calculated and returned 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}