#==========================================================================
# R Programming Assignment 2
# This creates cache matrix object by overriding the getter/setter
# Call via:
#   data<-c(4, 3, 3, 2)
#   mdata<-matrix(data, nrow=2, ncol=2)
#   mk<-makeCacheMatrix(mdata)
#   cacheSolve(mk)  #this will compute the inverse and cache the result
#   cacheSolve(mk)  #this will return the cached result
#==========================================================================


#-------------------------------------------------------------
# Define the cache matrix object with getter and setter overrides
#-------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(cacheSolve) m <<- cacheSolve
  getinverse <- function() m
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#-------------------------------------------------------------
## Return a matrix that is the inverse of 'x'
## If the inverse already exists, return the cached value,
## otherwise, calculate the inverse and cache the result.
#-------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  message("creating cached data")
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

