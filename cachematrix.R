## The functions gives inverse of a matrix. If inverse is already calculated. It will retrieve the results without calculations

## This function gives a matrix which has the inverse value of a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  sinverse <- function(inverse) m <<- inverse
  ginverse <- function() m
  list(set = set,
       get = get,
       sinverse = sinverse,
       ginverse = ginverse)
}

## The following function gives the inverse of a matrix. If the inverse is already calculated ( by is.null command) then cacheSolve gives inverse directly

cacheSolve <- function(x, ...) {
	m <- x$ginverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$sinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
