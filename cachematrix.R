## Create "matrix" and get it's inversion

## create "matrix" object supporting cached inversion

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  set_inverted <- function(inverted) inv <<- inverted
  get_inverted <- function() inv
  list(set = set, get = get,
       set_inverted = set_inverted,
       get_inverted = get_inverted)
}


## Calculate matrix iversion and cache result

cacheSolve <- function(x, ...) {
  inverted <- x$get_inverted()
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data, ...)
  x$set_inverted(inverted)
  inverted
}
