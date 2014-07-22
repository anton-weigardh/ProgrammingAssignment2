## cachematrix.R
## This script contains two functions, makeCacheMatrix and cacheSolve.
## A given matrix is cached. Later, the inverse of the matrix is calculated.
## The functions takes a matrix, x and calculates the inverse of said matrix, x^-1.
## In order to execute properly, the matrix must be invertible (i.e not singular and of size NxN, N = {2,3,...})

## In makeCacheMatrix, a "special" matrix is created that can cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y     
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## In cacheSolve, the inverse of the matrix is computed
cacheSolve <- function(x, ...){
  m <- x$get_inverse()
  if(!is.null(m)){
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

# Test script with, for example,
#mat <- matrix(rnorm(9),3)
#mat2 <- matrix(c(1,1,2,3),2)
#cacheSolve(makeCacheMatrix(mat))
#cacheSolve(makeCacheMatrix(mat2))