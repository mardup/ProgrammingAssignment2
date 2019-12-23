## This assignement is to create 2 functions to cach the inverse of a matrix 
## similarly to the example to cache mean

## This function is to set matrix value (setM), get matrix value (getM)
## set the matrix inverse/solve (setSolve) and get the matrix inverse/solve (getSolve)
makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  CacheM <- NULL
  setM <- function(y) {
    x <<- y
    CacheM <<- NULL
  }
  #get the value of the matrix
  getM <- function() x 
  #set the inverse of the matrix
  setSolve <- function(solve) CacheM <<- solve
  #get the inverse of the matrix
  getSolve <- function() CacheM
  list(setM = setM, getM = getM,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function will check if inverse of the matrix has been already calculated
## if not it will calculate, if it has it will send the result 
## with print message "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  CacheM <-x$getSolve()
  if(!is.null(CacheM)) {
    message("getting cached data")
    return(CacheM)
  }
  #calculate the inverse
  data <- x$getM()
  CacheM <- solve(data, ...)
  x$setSolve(CacheM)
  return(CacheM)
}
