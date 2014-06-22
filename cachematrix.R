## Put comments here that give an overall description of what your
## functions do

## This function creates and returns a matrix that can be cached
## in order to improve computing time. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolvedmatrix <- function(solve) m <<- solve
  getsolvedmatrix <- function() m
  list(set = set, get = get,
       setsolvedmatrix = setsolvedmatrix,
       getsolvedmatrix = getsolvedmatrix)
}


## this function solves (inverses) a matrix created in makeCacheMatrix()
## if first checks to make sure the matrix has not been solved. If it is in the cache
## it returns hence saving the calculation

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getsolvedmatrix()
    if(!is.null(m)) {
      message("getting cached solved matrix")
      return(m)
    }
    a_matrix <- x$get()
    m <- solve(a_matrix, ...)
    x$setsolvedmatrix(m)
    m
}
