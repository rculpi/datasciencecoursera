## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to
## matrix inversion that we will not discuss here). Your assignment is to write a pair 
## of functions that cache the inverse of a matrix.

## Write the following functions:
  
## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Testing de funcions

my_matrix <- matrix(rnorm(25),5,5)
my_matrix_cached <- makeCacheMatrix(my_matrix)
cacheSolve(my_matrix_cached)

## [,1]       [,2]       [,3]        [,4]       [,5]
## [1,]  0.243659392 -0.4429717 -0.4985223 -1.65642020  0.4645730
## [2,]  0.033708219  0.6971282  1.0428226  1.14785339 -1.4920296
## [3,] -0.604838774  0.6280577  0.2140196  1.47902504  0.7852962
## [4,] -0.260374914 -0.1351449  0.3420485 -0.04331824 -1.4472900
## [5,] -0.004439894 -0.1412987  1.0594317  0.28799940 -1.0416161

cacheSolve(my_matrix_cached)

## getting cached data

## [,1]       [,2]       [,3]        [,4]       [,5]
## [1,]  0.243659392 -0.4429717 -0.4985223 -1.65642020  0.4645730
## [2,]  0.033708219  0.6971282  1.0428226  1.14785339 -1.4920296
## [3,] -0.604838774  0.6280577  0.2140196  1.47902504  0.7852962
## [4,] -0.260374914 -0.1351449  0.3420485 -0.04331824 -1.4472900
## [5,] -0.004439894 -0.1412987  1.0594317  0.28799940 -1.0416161
