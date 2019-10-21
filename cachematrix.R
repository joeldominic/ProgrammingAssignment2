## The makeCacheMatrix is there to store the matrix and the function in a different environement which can be called on later from another function.
## The cacheSolve first computes the inverse of a matrix before it stores it in a variable. 
## After it is stored, if the same matrix needs to be inversed, it will just get from the stored value

## This makeCacheMatrix function accepts a matrix as an argument. 
## That matrix is stored in 'x' which is in a different environment. 
## The set, get, setinverse, getinverse functions are all in a different environment

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## After storing the matrix in 'x', the cacheSolve function returns the inverse after doing the solve computation and stores it in 'm'.
## If the same matrix is passed again, it will first check if it was previously stored in 'm'.
## If it has been stored previously, it will not recompute. It will just get the inverse that was saved in 'm'

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if (!is.null(m)) {
        print("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
