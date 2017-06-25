## makeCacheMatrix and cacheSolve functions both enable the cache of the inverse of a matrix

## makeCacheMatrix function specifically creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y)  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve function calculates the inverse of the special matrix returned by makeCacheMatrix as discussed above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()     
  if(!is.null(inverse))   {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inv)
  inv      
}
