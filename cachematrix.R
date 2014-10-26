# The first function caches the inverse of a matrix.  The second function
# solves for the inverse after first checking with the previous function to see  # if the inverse has already been solved.

# This first function instructs R to create a cache of the inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
  x <<- y
  i <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This second function looks to see if the inverse of matrix x has already been # cached (in which case the message "getting cached data" is returned.)  If not # the function calculates the inverse of matrix x.


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
}
