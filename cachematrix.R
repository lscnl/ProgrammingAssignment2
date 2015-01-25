## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## functions to get and set the inverse (solve) of a matrix 
## added function to find the determinant
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  chkdet     <- function(det) m <<- det
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       chkdet = chkdet,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
## returns a matrix that is the inverse of x, also checks that the matrix is square and has an inverse
## use the functions in makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if(!(nrow(data) == ncol(data))) {
    message("matrix is not square, unable to solve")
    return(data)
  }
  m <- det(data)
  if(m == 0) {
    message("determinant is zero, unable to solve")
    return(m)
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
