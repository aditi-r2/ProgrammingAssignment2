## Put comments here that give an overall description of what your
## functions do

#MakeCacheMatrix#
makeCacheMatrix <- function(x = matrix()) {
  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m1 <<- solve
  getinverse <- function() m1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#CacheSolve#
cacheSolve <- function(x, ...) {
  m1 <- x$getinverse()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data, ...)
  x$setinverse(m1)
  m1
}


#Example#
a1<- diag(5,3)
a1

CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)

b1 <- diag(2,6)
b1

CachedMarix <- makeCacheMatrix(b)
cacheSolve(CachedMarix)

cacheSolve(CachedMarix)   #getting cached data
