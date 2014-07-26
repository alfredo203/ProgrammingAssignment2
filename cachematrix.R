## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   ##Prompt call the function
  m <- NULL                                  ##m is null to be overwriten
  set <- function(y) {                      #function inside function to overwrite m, star the cycle with x<-y
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) m <<- inverse1
  inverse2 <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       inverse2 = inverse2)
}                                         #Set function its finished with m, x, y 


cacheSolve <- function(x, ...) {        #Second function, supossed to take matrix x and solve it from cache
  m <- x$inverse2()
  if(!is.null(m)) {
    message("loading from cache")      ##Caché message, shows that its taking from caché
    return(m)
  }
  data <- x$get()                       ## x$get its 
  m <- solve(data, ...)               ##Solve the inverse matrix from the caché in data
  x$setinverse(m)
  m        ## m as the inverse matrix of x
  
}