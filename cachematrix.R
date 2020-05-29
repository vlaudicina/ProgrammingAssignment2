## Write 2 functions - 1 that creates a special matrix that can
## cache its inverse and the other that computes the inverse of the 
## special matrix. 

## Creates special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the special matrix from makeCacheMatrix. If inverse has already
## been calculated, then this function should retrieve the inverse from the 
## cache

cacheSolve <- function(x, ...) {
 m <- x$getinverse()
 if (!isnull(m)) {
   message("getting cached data")
   return(m)
 }
 data <- x$get()
 m <- solve(data,...)
 x$setinverse(m)
 m
}
