## Function makeCacheMatrix
## Aim: To create a special "matrix" object that can cache its inverse.
## Input: x which is a matrix (assume that it is invertible).
## Output: A list containing four functions to set and get the value of matrix and its inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # i = set to NULL to clear previous value
    # declaring it as an object within the makeCacheMatrix environment
    set <- function(y) {
            x <<- y             # setting the value
            i <<- NULL          # clears previous cache
}
    get <- function() x  # Function to get the value of matrix
    
    # Function to set the inverse when there is no cache inverse
      setinverse <- function(inverse) i <<- inverse
    # Function to get the inverse
      getinverse <- function() i
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
    }

## Function cacheSolve
## Aim: To calculate the inverse of matrix from makeCacheMatrix; it will also retrieve calculated inverse if the matrix has not changed
## Input: x is a list which is an output of makeCacheMatrix that contains 4 set of functions
## Output: An inversed matrix of x

cacheSolve <- function(x, ...) {
        # Get the cached value of the inversed matrix
        i <- x$getinverse()
        # If there is no value, then the matrix needs to be inversed
        if (!is.null(i)){
                  message("getting cached data")
                  return(i)
          }
        # Get the matrix value
          data <- x$get()
          i <- solve(data, ...)
        # Cache the inversed value
          x$setinverse(i)
        # Returning the value
          return(i)
        }
# Testing
testmatrix <- matrix(c(4,2,1,7),2,2)

a = makeCacheMatrix(testmatrix)

cacheSolve(a)
