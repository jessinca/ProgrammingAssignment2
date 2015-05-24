## 
# Creates a special "list" to facilitate cached computation of a square matrix inverse
# 
# Args:
# x: a square matrix (a matrix with same number of rows and columns)
#
# Returns:
# a list containing a function to :
# 1. set the value of the square matrix
# 2. get the value of the square matrix
# 3. set the value of the inverse of the square matrix
# 4. get the value of the inverse of the square matrix
#
##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##
# Computes the square matrix inverse of the special "list" created with makeCacheMatrix function
# If the matrix inverse has been computed before, 
# the function will return the previously computed value stored in the cache 
# Args :
# x : output of makeCacheMatrix function
## Returns : a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 
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
