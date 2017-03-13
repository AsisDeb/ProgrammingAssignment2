## This function creates a list of functions to set and get the matrix and 
## also set and get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix ()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  z = list(set = set, get = get, setinv = setinv, getinv = getinv)
  cacheSolve(z)
}
## This function returns the inverse of the matrix passed from the 
## makeCacheMatrix function after checking whether the inverse exists
## and whether the matrix has changed
cacheSolve <- function(z) {
  inv <- z$getinv()
  if(!is.null(inv) && z$get() != z$set) {
  message("getting cached data")
  return(inv)
  }
  data <- z$get()
  inv <- solve(data)
  z$setinv(inv)
  inv
}