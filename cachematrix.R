#Peer graded asssignment

makeCacheMatrix <- function(x = matrix()) {
  # assume that all the matrix given are invertible
  inv <- NULL
  #setting values of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
    inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Returning a matrix 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data(inverse of matrix present)")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
