## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) xInv <<- solve
  getInv <- function() xInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInv()
  if(!is.null(xInv)) {
    message("getting cached matrix inverse")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInv(xInv)
  xInv
}
