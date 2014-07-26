## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) m <<- inverse1
  inverse2 <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       inverse2 = inverse2)
}


cacheSolve <- function(x, ...) {
  m <- x$inverse2()
  if(!is.null(m)) { ##Check whether cached
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}