## These functions allow for the inverse of a matrix to be cached to increase speed 
## in cases where the inverse may be aksed for multiple times.

## This function creates the cache matrix object

makeCacheMatrix <- function(mat = matrix()) {
  # initialize the inverse to NULL
  inv <- NULL
  
  # function to set the matrix.  This resets the inverse to null
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  
  #  function to return the original matrix
  get <- function() mat
  
  # function to store the inverse of the matrix in the inv variable and thus caching it
  setInv <- function(inverse) inv <<- inverse
  
  # function to return the cached inverse
  getInv <- function() inv
  
  # returns a list of the internal functions defined for this environment
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function looks up if the inverse has already been calculated and returns it, 
## otherwise it calculates the inverse, stores it in cache and then returns it

cacheSolve <- function(x, ...) {
  #  grab out the cached inverse
  inv <- x$getInv()
  
  # check if the cached inverse actually existed and if so return it
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if we reach this code this means the cached inverse did not exist so we grab the original matrix
  data <- x$get()
  
  # use the solve funciton to calculate the inverse of the matrix
  inv <- solve(data)
  
  #  store the inverse in cache
  x$setInv(inv)
  
  #return the inverse
  inv
}
