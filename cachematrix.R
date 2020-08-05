## Group of two functios made to create a cache matrix that will contain a field to store 
## it's inverse 

## Function that creates a cache matrix

makeCacheMatrix <- function (x = matrix()){
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL 
      }
      get <- function() x
      setInverse <- function(i) inverse <<- i
      getInverse <- function() inverse
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## function that calculates the inverse of that matrix, but only if it doesn't have the value 
## of the inverse already calculated

cachesolve <- function(x, ...){
      inverse <- x$getInverse()
      if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
      matrix <- x$get()
      inverse <- solve(matrix, ...);
      x$setInverse(inverse); 
      inverse
}