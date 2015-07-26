## These scripts calculate the inverse matrix for a new matrix 
## or retrieve from the cache if it is already calculated

## This function creates a matrix object that can cache its inverse
## It is a list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  ## x must be a square invertible matrix
  
  matrix.inv= NULL
  
  set = function(y) {
    ## "<<-" is used to assign a value to an object in an env
    ## that is different from the current env
    x <<- y
    matrix.inv <<- NULL
  }
  
  get = function() x
  
  setinv = function(inverse) matrix.inv <<- inverse 
  
  getinv = function() matrix.inv
  
  list(set =set, get =get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse matrix 
## of the matrix created above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix.inv = x$getinv()
  
  ## If the result is already calculated
  if(!is.null(matrix.inv)){
    message("retrieving from cache")
    return(matrix.inv)
  }
  
  new_matrix= x$get()
  matrix.inv = solve(new_matrix,...)
  
  x$setinv(matrix.inv)
  
  return(matrix.inv)
}
