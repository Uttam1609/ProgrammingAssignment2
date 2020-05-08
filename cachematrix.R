## Put comments here that give an overall description of what your
## functions do
## These functions create a 'special matrix' which can hold the matrix object as well as
## its inverse in the memory (cache)

## Write a short comment describing this function
## This function creates a special matrix which is like a list containing various objects

makeCacheMatrix <- function(x = matrix()) {
 inv_mat <- NULL
 set <- function(y){
   x <<- y
   inv_mat <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) inv_mat <<- inverse
 getinverse <- function() inv_mat
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
## This function calulates the ivnverse of the special matrix created in the above function. 
## It first chects whether inverse has already been calculated.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!(is.null(inverse))){ # finds if the inverse is already calculated and if true, it skips 
                                    # further calculation
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
