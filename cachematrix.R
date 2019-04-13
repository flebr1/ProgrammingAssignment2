#
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# These functions help creating instances of "special matrix" that allow to cache 
# the inverted matrix so that multiple request for the inverted matrix won't 
# systematically retrigger the entire matrix inversion (CPU intensive) process.
#

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) i <<- inv
  
  getinv <- function() i
  
  list(
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv
    )
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

#
# to test (example)
# 
# > a = matrix(c(1, 2, 3, 4), 2, 2)
# > a
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# > b = solve(a)
# > b
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > a %*% b
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1
#
#### source(...) !! use appropriate path to cachematrix.R source file in your env
#
# > a_better <- makeCacheMatrix(a)
#
# > a_better$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#
# > cacheSolve(a_better)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
# > cacheSolve(a_better)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
