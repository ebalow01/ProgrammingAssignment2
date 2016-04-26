## Programming assignment 2 Lexical Scoping
## ebalow01

## Finds the inverse of a matrix, using a cached result if necessary.

## usage example:
## mymatrix <- makeCacheMatrix(rbind(c(1,-1/4),c(-1/4,1)))
## cacheSolve(mymatrix)
## cacheSolve(mymatrix)

## makeCacheMatrix creates a function which performs operations
## to create and solve an inversible matrix
## parameter:  x of type matrix which contains an inversible matrix
## returns a vector containing the function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## cacheSolve checks the cache to see if the matrix has been solved.
## if so it provides the cached result, otherwise it computes the inverse
## parameter: x is the function previously set to the inversible matrix
## returns the inverse of the matrix previously set

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

