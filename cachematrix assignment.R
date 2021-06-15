## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

## this function creates a special"matrix object that can cache its inverse

makeCacheMatrix <- function(x =matrix())  ## define the argument with default mode of "matrix"
  inv <- NULL                             ## initializes inv as NULL
  set <- function(y)                      ## define the set function to assign new
      x <<- y                             ## value of matrix in parent environment
      inv <<- NULL                        ##reset inv to NULL if there be any new matrix 
      
  get <- function()x                  ## returns value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                        ## to the functions with the $ operator
}
  
  
  
  
  


## Write a short comment describing this function
## this function computes the inverse of the special "matrix" 
##if the inverse has already been calculated and matrix hasn't change, cachesolve will retrieve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

