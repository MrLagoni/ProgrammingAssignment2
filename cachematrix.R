### Overall description
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. To do this, the following functions can be applied:
#   
# `makeCacheMatrix`:  Creates a special "matrix" object that can cache 
#                     its inverse.
# `cacheSolve`:       Computes the inverse of the special "matrix" returned
#                     by `makeCacheMatrix` above. 

### Functions

# The function, `makeCacheMatrix` creates a special "matrix", which is
# a list containing functions to
# 
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse
# 
# It is assumed that the input matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The function 'cacheSolve' computes the inverse of the special
# "matrix" returned by `makeCacheMatrix`. First, however, the function 
# checks if the inverse has already been calculated. If so, it 'get's 
# the inverse from the cache and skips the computations. If not, then
# `cacheSolve' calculates the inverse and sets the value of the inverse
# in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
 
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

### Examples
# A <- matrix(1:4, nrow = 2)
# 
# cacheA <- makeCacheMatrix(A)
# 
# cacheA$get()
# #      [,1] [,2]
# # [1,]    1    3
# # [2,]    2    4
# 
# cacheA$getinverse()
# # NULL
# 
# cacheSolve(cacheA)
# #      [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5
# 
# cacheSolve(cacheA)
# # getting cached data
# #       [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5