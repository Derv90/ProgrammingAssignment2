## Assignment - Caching the Inverse of a Matrix 

## 1. MakeCacheMatrix - creates a special matrix object that can cache its inverse rather than compute it repeatedly;
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2. CacheSolve - takes the special matrix output by the MakeCacheMatrix about and computes the inverse. 
# In the case where the inverse has already been calculated, CacheSolve should get the inverse from the cache.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}

X <- matrix(c(1,2,3,4),2,2)
X1 <- makeCacheMatrix(X)
X
X1
cacheSolve(X1)
