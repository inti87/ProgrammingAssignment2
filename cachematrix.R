## Caching the Inverse of a Matrix

## Functions below:
##   - first: creates a special "matrix" object that can cache its inverse
##   - second: computes the inverse of the special "matrix" returned by the first function
##     if the inverse exists (and matrix is not change) then the function returns the inverse from cache
##     otherwise it calculates the inverse for given special "matrix"


## Function for creating special "matrix"
## - (matrix must be square and invertible in order to calculate its inverse!
## - here we assume that supplied matrix is always invertible!)
## - function returns a list to
##   a) set the value of the matrix
##   b) get the value of the matrix
##   c) set the value of the matrix inverse
##   d) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           # inverse of matrix (set to NULL)
  set <- function(y) {  # set the matrix object (in cache)
    x <<- y
    inv <<- NULL
  }
  get <- function() x                          # get the values of matrix (from cache)
  setinv <- function(inverse) inv <<- inverse  # set the inverse of a matrix (in cache)
  getinv <- function() inv                     # get the inverse of a matrix (from cache)
  
  list(set = set, get = get,     # store collection of functions in a list
       setinv = setinv,
       getinv = getinv)
}


## Function for calculating the inverse of "special" matrix (created with above function)
## - it computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  - if the inverse has already been calculated 
#  - then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv   <- x$getinv() # get inverse of matrix (if exists)
  if(!is.null(inv)) { # check if inverse already exists
    message("getting cached data")
    return(inv)
  }
  
  M <- x$get()         # get matrix 
  inv <- solve(M, ...) # calculate the inverse of new matrix
  x$setinv(inv)        # cache inverse of new matrix
  inv                  # return new inverse
}
