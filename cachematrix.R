## These two function are used to calculate the inverse of a matrix or to get the cached inverse matrix if already calculated.

## This function creats a list of function allowin to set and get a matrix and/or its inverse. The values of the matrix and inverse of the matrix are cached in the makeCachematrix function environment.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function uses the list of function created by the makeCacheMatrix and returns the inverse of the matrix by getting the cache value or calculating the inverse matrix if no value is already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
