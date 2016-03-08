## This function creates a "special matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setmatrix <- function(matrix) {
    m <<- matrix
  }
  
  getmatrix <- function() {
    m
  }
  
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        data <- x$get()
        
        if (!is.null(m) & identical(m, data)) {
          message("Getting cached matrix...")
          return (m)
        }
        
        m <- solve(data, ...)
        
        x$setmatrix(m)
        
        m
}
