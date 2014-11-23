## cachematrix is a "matrix" which "lazily" caches the resulting inverse of the matrix
## It is "Lazy" because the inverse is only cached upon request (via the cacheSolve function)

## Creates a matrix cache object which has getters/setters to store the supplied matrix 
## and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## A version of solve which works with a "cachematrix". If the inverse matrix has already been
## calculated it will be returned from the cached answer

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
