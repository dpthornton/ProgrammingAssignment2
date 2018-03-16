## The following functions compute the inverse of a matrix
## if a cached version of the inverted matrix does not
## already exists. If already exists then the cached version is
## returned which prevents unnecessary recalculation.
## The matrix must be a square invertible matrix.

## Returns a list of functions for storing and returning a matrix
## Input: a square invertible matrix 
makeCacheMatrix <- function(the_matrix = matrix()) {

  cached_inv_matrix <- NULL
  
  set <- function(y) {
    the_matrix <<- y
    cached_inv_matrix <<- NULL
  }
  
  get <- function() the_matrix
  
  setMatrixInverse <- function(inv_matrix) cached_inv_matrix <<- inv_matrix
  
  getMatrixInverse <- function() cached_inv_matrix
  
  list(set = set, 
       get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Input: makeCacheMatrix instance
## Return a matrix that is the inverse of 'x'
## 1. Attempts to get a cached version of the inverted matrix
## 2. If the cached version has not been set, recompute and store 
## 3. return the inverted matrix
cacheSolve <- function(make_cache_matrix, ...) {
        
  inv_matrix <- make_cache_matrix$getMatrixInverse()
  
  if(is.null(inv_matrix)) {

    message("computing matrix inverse")
    
    the_matrix <- make_cache_matrix$get()
    
    inv_matrix <- solve(the_matrix, ...)
    
    make_cache_matrix$setMatrixInverse(inv_matrix)
  } 
  
  return(inv_matrix)
}

####################################################
## BASIC TEST SCRIPT, RUN IN THIS ORDER ############
rm("test_matrix","test_make_cache_matrix","test_inv_matrix")
test_matrix <- matrix(data = rep(1:4), 2, 2)
test_make_cache_matrix <- makeCacheMatrix(test_matrix)
test_inv_matrix <- cacheSolve(test_make_cache_matrix)
## Test the matrix inversion function matches the result of solve
class(solve(test_matrix)) == class(test_inv_matrix)
solve(test_matrix) == test_inv_matrix
## run again, this time the matrix should be retrieved from cached version
test_inv_matrix <- cacheSolve(test_make_cache_matrix)
## Test again the matrix inversion function matches the result of solve
class(solve(test_matrix)) == class(test_inv_matrix)
solve(test_matrix) == test_inv_matrix
####################################################