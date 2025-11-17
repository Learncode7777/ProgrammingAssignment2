## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Overall description:
## These functions create a special "matrix" object that can cache its inverse,
## and compute the inverse of the matrix. If the inverse has already been calculated
## and the matrix has not changed, the cached inverse is retrieved to save computation.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL   # Initialize the inverse as NULL
  
  # Function to set a new matrix and reset the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" from makeCacheMatrix
## If the inverse is already cached, it retrieves it from the cache
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()   # Get the cached inverse
  
  # If inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()          # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)       # Cache the inverse
  inv                      # Return the inverse
}

## Example usage:
## m <- makeCacheMatrix(matrix(c(2,0,0,2), 2, 2))
## cacheSolve(m)  # Computes the inverse
## cacheSolve(m)  # Retrieves the cached inverse with message "getting cached data"

