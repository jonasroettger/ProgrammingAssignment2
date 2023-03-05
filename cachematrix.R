

## This funcion calculates the invers and it can be used to re-asign this to a new object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function evaluates the object that is created using makeCachMatrix function. It returns the Inverse. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



# Example 
mat1.data <- c(23, 60, 2, 3.5, 17, 25, 50, 100, 11)
mat1 <- matrix(mat1.data,nrow=3,ncol=3,byrow=TRUE)
mat1

inverse <- makeCacheMatrix(mat1)
cacheSolve(inverse)
