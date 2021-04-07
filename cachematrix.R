# Programming in R
# Assignment 2

################################################################################

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly (there
# are also alternatives to matrix inversion that we will not discuss here). Your
# assignment is to write a pair of functions that cache the inverse of a matrix.

################################################################################

# Create a chached matix enviornment stored in at memory address

makeCacheMatrix <- function(A = matrix()) {
  
  # Initialize I the inverse matrix and clear the memory address environments
  I <- NULL
  
  set <- function(A1) {
    # Pass A1 matrix to A memory address
    A <<- A1
    
    # Initialize I the inverse matrix and clear the memory address environments
    I <<- NULL
  }
  get <- function() A
  
  setinv <- function(inv) I <<- inv
  
  getinv <- function() I
  # Return list of functions/data from associated memory address environments
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Solves for inverse of cached matrix store in memory address environment
# returned by makeCacheMatrix.

cacheSolve <- function(A, ...) {
  # Return a matrix that is the inverse of 'A'
  I <- A$getinv()
  
  # Return inverse if it already exists
  if(!is.null(I)) {
    return(I)
  }
  
  # subset A from list returned in makeCacheMatrix function
  Asub <- A$get()
  
  # solve for inverse of A
  I <- solve(Asub)
  
  # assign I to cached list
  A$setinv(I)
  
  # return inverse to console
  I
}


