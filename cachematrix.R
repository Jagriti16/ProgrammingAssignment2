##This function is going to create a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) 
{
  
  i <- NULL  
  
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() 
  {
    x 
  }
  setinverse <- function(inverse)
  { 
    i <<- inverse
  }
  
  getinverse <- function()
  {
    i
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

##This function returns inverse of matrix 'x' with the help of data provided by the cache matrix function but 
##before returning the matrix it checks if the values are already present in the memory, if yes then it prints the 
##data from the memory.If the data isn't present in the memory then inverse of matrix x is performed by the solve function.
cacheSolve <- function(x, ...) 
{
  i <- x$getinverse() 
  
  if (!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
