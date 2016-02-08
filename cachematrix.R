## Matrix with Inverse cached
## set(y), set matrix
## get(), return matrix
## setInv(data), set inverse
## getInv(), return inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(data) inverse <<- data
  getInv <- function() inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Return a matrix that is the inverse of 'x'
## If not exist cached inverse, it is calculated and saved
cacheSolve <- function(x, ...) {
  
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}


