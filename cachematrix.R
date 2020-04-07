## Put comments here that give an overall description of what your
## functions do

## Creates a special vector which is really a list to set the value of the matrix, get the value of the matrix, set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## actually calculates inverse of the matrix created by the above function
## if already calculated, gets inverse from the cache
## else actually calculates and sets the value in the cache

cacheSolve <- function(x, ...) {
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
