
makeCacheMatrix <- function(x = matrix()) {
  d  <- NULL
  set  <- function(y){
    x <<- y
    d <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) d  <<- inverse
  getinverse  <- function() d
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## cache uses information that has been saved previously in order to do the job. it is necesary for do several things at the same time 

cacheSolve <- function(x, ...) {
  d  <- x$getinverse()
  if (!is.null(d)){
    message("getting cached data")
    return(d)
  }
  data  <- x$get()
  d  <- solve(data, ...)
  x$setinverse(d)
  d
}
