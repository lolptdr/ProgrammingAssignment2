## Cache Inverse of a Matrix file combines the making of inverse matrix (set and get value of matrix, 
## set and get value of inverse) and returning of the inverse matrix if it has been calculated already 
## (otherwise it calculates and caches it).
## Assumption: matrix is invertible
## by Joseph Tingsanchali
## Coursera.org's Data Science specialization by Johns Hopkins University
## 22 April 2014

##  makeCacheMatrix function creates a special "matrix" object that can cache its inverse, using four functions: 
## get,set,setSolve,getSolve.

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


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## retreives the inverse from the cache.

cacheSolve <- function(x, ...) {
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
