## This file contains two functions for calculating and caching inverse of a matrix 


## makeCacheMatrix gets and sets the value of a matrix and its inverse in the environment
makeCacheMatrix <- function(x = matrix()) 
  {
      ## Set the inverse to NULL initially to begin with
      inverse <- NULL
      ## Set the value of x and inverse to NULL in the calling environment of set 
      set <- function(y) {
        x <<- y
        inverse <<- NULL
      }
      ## Return the value of x
      get <- function() x
      ## Set inverse as passed in to the inverse variable in calling environment
      setinverse <- function(inv) inverse <<- inv
      ## Return cached value of inverse 
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
  }


## cacheSolve checks the cache for inverse of a matrix and calculates it if not found
cacheSolve <- function(x, ...) 
  {
      ## Checks if inverse of x is cached in the environment 
      inverse <- x$getinverse()
      ## If inverse of x is found in cache
      if(!is.null(inverse)) {
        message("getting cached data")
        ## Return the cached inverse of x and exit the function
        return(inverse)
      }
      ## If inverse is not found in cache calculate it and set it in the cache
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      ## Return the inverse of 'x'
      inverse
}
