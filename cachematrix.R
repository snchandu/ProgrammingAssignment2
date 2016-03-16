## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix set the Matrix, Get the Matrix, Set the Inverse and Getthe Inerse
makeCacheMatrix <- function(x = matrix()) {
  ## Input: x: A square invertible matrix
  ## return: a list containing functions to
  ## (i)    Set the matrix
  ## (ii)   Get the matrix
  ## (iii)  Set the inverse
  ## (iv)   Get the inverse
  ##  (v) The New list is used as the input to cacheSolve() function
  inv <- NULL
  set <- function(y) {
    # use "<<-" to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Create Inverse of the original Matrix input to makecacheMatrix
## If doesn't exists in the  Cache, creates one for us 

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv = x$getinv()
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  return(inv)
}
