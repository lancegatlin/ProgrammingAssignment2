## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a list of 4 functions for getting or setting the value of the matrix cache
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Write a short comment describing this function
## Accepts a matrix cache and either returns the previously computed matrix inverse from the cache or computes the inverse and stores it in the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
