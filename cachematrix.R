## cachematrix.R
## This script contains two functions, makeCacheMatrix and cacheSolve.
## A given matrix is cached. Later, the inverse of the matrix is calculated.
## The functions takes a matrix, x and calculates the inverse of said matrix, x^-1.
## In order to execute properly, the matrix must be invertible (i.e not singular and of size NxN, N = {2,3,...})


## In makeCacheMatrix, a "special" matrix is created that can cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()){
  val <- NULL
  set <- function(y){
    x <<- y     
    val <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) val <<- solve
  get_inverse <- function() val
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## In cacheSolve, the inverse of the matrix is computed. 
## The function first checsk if the inverse of the matrix has already been computed.
## If not already computed, the function returns the inverse.
cacheSolve <- function(x, ...){
  val <- x$get_inverse()
  if(!is.null(val)){
    message("Getting cached data...") # Returns cached inverse.
    return(val)
  }
  data <- x$get()
  val <- solve(data, ...) # Calculate the inverse
  x$set_inverse(val)
  val
}

## Example ##
## Test script with, for example
mat <- matrix(c(1,1,2,3),2)
m1 <-(makeCacheMatrix(mat))
cacheSolve(m1)
cacheSolve(m1)

## Output
#> mat <- matrix(c(1,1,2,3),2)
#> m1 <-(makeCacheMatrix(mat))
#> cacheSolve(m1)
#[,1] [,2]
#[1,]    3   -2
#[2,]   -1    1
#> cacheSolve(m1)
#Getting cached data...
#[,1] [,2]
#[1,]    3   -2
#[2,]   -1    1