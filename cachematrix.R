##this function will reduce the time taken to solve the inverse of a matrix

## this part will get the inverse if already saved in cache

makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y){
    x <<- y
    f <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) f <<- inverse
  getInverse <- function() f 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## if not fount in cache then this part will solve inverse and return also save in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  f <- x$getInverse()
  ##checkin in cache memory
  if(!is.null(f)){    
    message("getting cached data")
    ## Returning from cache
    return(f)
  }
  mat <- x$get()
  f <- solve(mat,...)   ##Solving the inverse
  x$setInverse(f)     ##Inserting data into cache for future use
  f       ##returning the inverse 
}
##function complete
