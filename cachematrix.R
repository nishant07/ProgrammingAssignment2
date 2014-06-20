## R script defining function to create a matrix and find the inverse of that.
## Script show high performance by caching the inverse of matix and reusing whenever required.

## Function for setting and getting the matrix vector.

makeCacheMatrix <- function(x = matrix()) {
  
  ##Declare Inverse matrix as NULL
  inv <- NULL
  
  ##Function for assigning the value of Matrix to var 'x' & its inverse.
  set <- function(y){
    ## <<- operator. Usage :  var <<- value. 
    ## This operator will cause the interpreter to first search through the current environment to find the symbol var. 
    ## If the interpreter does not find the symbol var in the current environment, then the interpreter will next search through the parent environment. 
    ## The interpreter will recursively search through environments until it either finds the symbol var or reaches the global environment.

    x <<- y
    inv <<- NULL
  }
  
  ##Getting the matrix
  get <- function() x
  ##Assigning the value to inverse of matrix
  setinv <- function(in_m) inv <<- in_m
  ##Getting the value of inverse of matrix
  getinv <- function() inv
  
  ##The function reutrns the child functions as a list.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to find inverse of matrix. If an inverse is found before already. It just return that value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Assigning value of inverse of matrix to var 'inv' returned by 'getinv' function
  inv <- x$getinv()
  
  ## Condition to check if the inverse of matrix was calculated before or not.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## Geting the value of matrix using function 'get'
  dat <- x$get()
  ## Finding the inverse of matrix
  inv <- solve(dat)
  ## Setting the value for inverse of matrix using 'setinv
  x$setinv(inv)
  ## Returning inverse of matrix
  inv
}
