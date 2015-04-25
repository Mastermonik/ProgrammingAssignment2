## makeCacheMatrix creates a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(y) {
      x <<- y
      inverse = NULL
    }
    get <- function() x
    setinverse2 <- function(inverse) inverse <<- inverse2
    getinverse2 <- function() inverse
    list(set = set, get = get,
         setinverse2 = setinverse2,
         getinverse2 = getinverse2)
  } 

## cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## Before doing so, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse2 function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse2()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse2(inverse)
  return(inverse)
}

