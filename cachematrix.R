##This function is going to create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  
  i <- NULL  ##This method initialises the inverse property
  
  set <- function(y) ##Method to set the matrix
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function()  ##Method to get the matrix
  {
    x  ##Returns the matrix
  }
  setinverse <- function(inverse) ##Method to set inverse of matrix
  { 
    i <<- inverse
  }
  
  getinverse <- function() ##Method to return inverse property
  {
    i
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##A list that returns all the methods
}

cacheSolve <- function(x, ...) 
{
  i <- x$getinverse() ##This function returns inverse of matrix 'x'
  
  if (!is.null(i)) ##This returns the matrix if it is already present in the memory
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get() ##Get the matrix from x
  i <- solve(data, ...) ##Calculation of inverse of matrix and storing in i
  x$setinverse(i) ##Set the inverse to the object
  i ##Returns the matrix
}