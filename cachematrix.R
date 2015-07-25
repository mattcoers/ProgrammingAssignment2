## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list of functions that perform setters and getters for the source matrix data as well as the inverse of the matrix data
# The solution for the inverse of the matrix is stored in the value "i"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  deleteinverse <- function() { i <<- NULL }
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() i <<- solve(x)
  setinverse()
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       deleteinverse = deleteinverse)
}


## Write a short comment describing this function
# This function checks for the value of "i" in the makeCacheMatrix object and if it exists, loads it, and prints the message, "getting cached matrix data"
# if the inverse does NOT exist, then it will calculate it and print the message "creating cached matrix data"
# in either case, once the inverse of the matrix is created or discovered, it returns the inversed matrix value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached matrix data")
    return(i)
  } else {
    message("creating cached matrix data")
    x$setinverse()
    i <- x$getinverse()
  }
  i
}

myData <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
dataSolved <- makeCacheMatrix(myData)
cacheSolve(dataSolved)

