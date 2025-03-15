## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invx <- matrix(, nrow = 1, ncol = 1)
  set <- function(y) {
    x <<- y
    invx <<- matrix(, nrow = 1, ncol = 1)
  }
  get <- function() x
  setinv <- function(inverse) invx <<- inverse
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!all(is.na(invx))) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
  
}
