
makeCacheMatrix <- function(x = matrix()) {
  
  ## This function makes a list containing a function to
  ## 1. Set the value of the matrix
  ## 2. Get the value of the vector
  ## 3. Set the value of the inverse
  ## 4. Get the value of the inverse
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
