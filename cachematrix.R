## 2014-07-27

## This pair of functions calculates the inverse of a square matrix. Upon the
## initial run of cacheSolve, the inverse of matrix x is calculated and cached
## in an object. If cacheSolve is run again for the same matrix x, the cached
## value will be used to save time.

## This function returns a list of four functions to:
# 1. set value of matrix
# 2. get the value of matrix
# 3. solve for and set the inverse of matrix
# 4. get the (cached) inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y # Special operator assigns value in environment outside of this "set" function.
          i <<- NULL # If new data is set, inverse is reassigned to NULL.
     }
     get <- function() x # Returns matrix that was used in call of makeCacheMatrix
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i # Not null if value has been cached
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function returns a matrix that is the inverse of x. However, after it is
## run once for a particular matrix 'x', the inverse matrix is cached in a
## variable outside of the scope of this environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
