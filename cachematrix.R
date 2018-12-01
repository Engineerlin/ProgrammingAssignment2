## This pair of function can cashe the inverse of a matrix.

## This function creates a special matrix that can cashe its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function coomputes the inverse of the special matrix returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated befor
## say it the same matrix has been called, then 'cacheSolve' should retrive
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
