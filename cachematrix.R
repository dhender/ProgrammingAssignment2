## Programming Assignment2 - R Programming
##
## Description:
## This R-script utilizes two functions (makeCacheMatrix,cacheSolve) to cache 
## the value of an invertable matrix so that when it is required again, it can 
## be looked up in the cache rather than recomputed. This approach is advantageous
## for code (especially inside loops) whereby the inversion calculation of a large 
## size matrix is only required once; avoidance of repetitive computations.
## 
## Example:
## source("cach_inv_matrix.R")
## m <- matrix(c(-1,3,-3,0,-6,5,-5,-3,1), nrow=3, ncol=3, byrow=TRUE)
## mcm <- makeCacheMatrix(m)
## cs <- cacheSolve(mcm)
## print(cs)
## cs should return:
##           [,1]      [,2]       [,3]
## [1,]  1.500000  1.000000 -0.5000000
## [2,] -4.166667 -2.666667  0.8333333
## [3,] -5.000000 -3.000000  1.0000000
##
## cs2 <- cacheSolve(mcm)
## This should display a "Getting cached matrix" message
## print(cs2)
## cs2 should return
##           [,1]      [,2]       [,3]
## [1,]  1.500000  1.000000 -0.5000000
## [2,] -4.166667 -2.666667  0.8333333
## [3,] -5.000000 -3.000000  1.0000000

makeCacheMatrix <- function(m = matrix()) {
  ## Using the analogy of the provided example (mean of a vector), makeCacheMatrix 
  ## creates a special "matrix" object, which is really a list containing a function to
  ## 1. set the matrix value
  ## 2. get the matrix value
  ## 3. set the inverse of the matrix
  ## 4. get the inverse of the matrix
  
  # Initialize
  inv <- NULL
  
  # set function
  # Sets the matrix itself but not the inverse
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  # get function
  # Gets the matrix itself but not the inverse
  get <- function() m
  
  # Manually set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Create a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(m, ...) {
  ## This function computes the inverse of the special "matrix" returned 
  ## by makeCacheMatrix above. If the inverse has already been calculated 
  ## (and the matrix has not changed), then the cachesolve should retrieve 
  ## the inverse from the cache.
  
  
  ## Compute inverse of matrix 'm' and cache its result
  
  # Obtain the current state of the inverse
  inv <- m$getinverse()
  
  # Determine if matrix inversion was executed 
  if(!is.null(inv)) {
    # return the inverse value		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If matrix inversion was not executed then get the matrix
  # itself
  data <- m$get()
  
  # Compute the inverse
  inv <- solve(data, ...)
  
  # Cache the result in the object
  m$setinverse(inv)
  
  # Return this new result
  inv    
}