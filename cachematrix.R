## These two functions work together to return the inverse of a matrix.
## The first function will save a copy of the inverse to the cache so that it can be used more
## quickly in later calls to the second function.

## This function creates a special "matrix" object that can cache its inverse
## It also creates a list of functions that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix 
## If the inverse has already been calculated, 
## it reads the value from the cache and does not calculate again.
## Otherwise, the function calculates the inverse of the matrix 
## and sets the value in the cache via the setinverse function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
