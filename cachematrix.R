## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## the inv matrix is assigned a null value
  set <- function(y) { ## store a matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##returns the stored matrix
  setinv <- function(inverse) 
    inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv) ##return a list. Each named element of the list is a function
  

}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  ## if a cached value exists return it
  if(!is.null(inv)) {
    message("getting cached result") ## Print this message when getting the cached value
    return(inv)
  }
  
  ## if the cached value does not exit
  ## get the matrix, caclulate the inverse 
  ## and store it in the cache
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv ##return the inverse
  
}
