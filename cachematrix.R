## Put comments here that give an overall description of what your
## functions do

## 
## This function creates a special "matrix" object that can cache its inverse.
## It returns a list containing 4 functions - a setter and getter for matrix
## and a setter and getter for Inverse
 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## if inverse is not null, get it from cache
  if(!is.null(inv)) {
    message("Retrieve Cached Data.")
    return(inv)
  }
  ## if inverse is null, calculate the inverse
  data <- x$get()
  inv <- solve(data)
  ## set the inverse in cache
  x$setinverse(inv)
  inv
}

 
