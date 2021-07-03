##
## Set the input x as a matrix
## Set "A" as a null
## tchange "mean" to "solve"
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  A <- NULL
  set <- function(y) {
    x <<- y
    A <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) A <<- solve
  getsolve <- function() A
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
cacheSolve <- function(x, ...) {
  A <- x$getsolve()
  if(!is.null(A)) {
    message("getting inversed matrix")
    return(A)
  }
  data <- x$get()
  A <- solve(data, ...)
  x$setsolve(A)
  A
}
