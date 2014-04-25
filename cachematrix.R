## Cache the inverse of a matrix

## The makeCacheMatrix function, creates a list, which containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedMatrix  <-  NULL
  
  set <- function(y) {
    x             <<- y
    cachedMatrix  <<- NULL
  }
  
  get        <- function() x
  setInverse <- function(solve) cachedMatrix <<- solve
  getInverse <- function() cachedMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix fixed with 
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
## the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  cachedMatrix <- x$getInverse()
  if(!is.null(cachedMatrix)) {
    message("getting cached data")
    return(cachedMatrix)
  }
  data         <- x$get()
  cachedMatrix <- solve(data, ...)
  x$setInverse(cachedMatrix)
  cachedMatrix
}
