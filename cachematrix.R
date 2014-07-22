## File: cachematrix.R
## Author: Weigardh,A
##
## This script contains two functions, makeCacheMatrix and cacheSolve.
## A given matrix's inverse is computed and cached.
##
## The functions takes a matrix, x and calculates the inverse of said matrix, x^-1.
## In order to execute properly, the matrix must be invertible (i.e not singular and of size NxN, N = {2,3,...})


## In makeCacheMatrix, a "special" matrix is created that can cache the inverse of the matrix.
## The list;
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix
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
## The function first checks if a inverse of the matrix has already been computed and. 
## In thais case, the cached inverse is returned.
## If not already computed, the function returns the inverse.
cacheSolve <- function(x, ...){
  val <- x$get_inverse()
  if(!is.null(val)){
    message("Getting cached data...") 
    return(val) # Returns cached inverse.
  }
  data <- x$get()
  val <- solve(data, ...) # Calculate the inverse
  x$set_inverse(val)
  return(val) # Returns the inverse of the matrix
}

## Examples
## Test script with, for example
#mat <- matrix(c(1,1,2,3),2)
#m1 <-(makeCacheMatrix(mat))
#cacheSolve(m1)
#cacheSolve(m1)

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