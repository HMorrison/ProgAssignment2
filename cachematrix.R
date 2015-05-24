## Compute the inverse of a matrix, using cached value if available.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x << - y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m << inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

