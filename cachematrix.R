## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object to cache its inverse.
##The <<- operator is used to allow the assignment of a value in an environment different from the current one
##First, we set the value of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  
##Afterwards we get the value of the Matrix
  get <- function() x                              
  
##and get the value of the inverted Matrix
  setInv <- function(inverse) matrix <<- inverse
  getInv <- function() matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

#The second function cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated and is identical to the matrix, then the inverse from the cached matrix is returned.
#First the inverse of 'x' is returned if inverse matrix is not Null

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  matrix <- x$getInv()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  
##Afterwards we continue the inverse of 'x', considering that the value of the invertible matrix is Null
  data <- x$get()
  matrix <- solve(data, ...)
  x$setInv(matrix)
  matrix
}