## makeCacheMatrix: This function creates a special "matrix" object
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  # inv will store the cached inverse matrix
  inv_x <- NULL
  
  # Setting the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  ## Getting the matrix
  get <- function() x
  
  ## Setting the inverse
  setinverse<- function(inverse) inv_x <<-inverse
  
  ## Getting the inverse
  getinverse <- function() inv_x
  
  ## Return the matrix with the defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## If the inverse has already been calculated, then the cachesolve should 
## retrieve the inverse from the cache.
## otherwise it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  
  ## If the inverse is already calculated, return it
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
    
    ## If The inverse is not yet calculated, so we calculate it
  } else {
    inv_x <- solve(x$get())
    
    ## Cache the inverse
    x$setinverse(inv_x)
    
    ## Return it
    return(inv_x)
  }
}