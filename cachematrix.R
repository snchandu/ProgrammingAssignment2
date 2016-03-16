## The goal here is to compute the inverse of an invertible square matrix.
## Also want to store it somewhere, so we don't have to compute the
## inverse more than once. Indeed, for large matrices,
## computing its inverse can take a lot of space and time.

## This function creates a new object, which consists of 4 functions that 
## can set and access to the value of a given matrix which is stocked outside 
## its environnement, the same for its inverse.

## makeCacheMatrix set the Matrix, Get the Matrix, Set the Inverse and Get the Inerse
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

## This function actually inverts a given matrix. The argument is this object
## we've defined before, so we can check if the inverse of this matrix have been
## already computed. In that case, the function the inverse and prints "getting
## cached data". Otherwise, it simply computes the inverse of the matrix and stores
## the inverse outside this environnement and returns the env.

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv <- x$getinv()
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  return(inv)
}
