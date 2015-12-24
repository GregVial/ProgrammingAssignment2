## Calculate the inversion of a matrix, but in a clever way
## Once operations has been performed once it will be cached 

## This function creates a vector of functions that get and set the matrix content
##   or get the set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function attempts to look if the inverse of the matrix has been 
##   calculated already, if yes then pull from memory, else compute it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinv(i)
    i
}
