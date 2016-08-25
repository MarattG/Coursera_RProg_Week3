## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse. This "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The function “makeCacheMatrix” creates a special matrix that can cache its oqninverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The function “cacheSolve” computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m  ## Return a matrix that is the inverse of 'x'
}