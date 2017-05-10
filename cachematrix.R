## The MakeCacheMatrix function creates a matrix and stores it in a
## cache. It has three usable nested functions.  The $get (using 
## "aMatrix$get()", for example) function can retrieve a cached
## matrix. The $set function can modify a matrix that has alredy
## been created. $getinverse can retrieve the inverse of a matrix,
## but only if it has already been calculated by the cacheSolve
## function. The cacheSolve function calculates the inverse of a matrix 
## that has been created by the makeCacheMatrix function, but first checks
## if the inverse has already been calculated. If so, it retrieves
## the inversed matrix from the cache.

## makeCacheMatrix creates and solves a cached matrix and manages the
## cache. Outputs are not automatically printed and must be retrieved
## using the two "get" functions.

makeCacheMatrix <- function(x = matrix(), a = nrow(), b = ncol()) {
  m <- NULL
  set <- function(y, c, d) {
    x <<- y
    a <<- c
    b <<- d
    m <<- NULL
  }
  get <- function() matrix(x, a, b)
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix after first
## checking if the inversed matrix has already been calculated.
## Outputs are automatically printed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}