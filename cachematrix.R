## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ##initialize the inverse
  i<- NULL
  
  #set the matrix
  set <- function(y)
  {
    x <<-y
    i <<-NULL
  }
  
  #Get the Matrix
  get <-function()
  {
    x
  }
  
  #set the inverse
  setInverse <- function(inverse)
  {
    i <<- inverse
  }
  
  #get the inverse
  getInverse <- function()
  {
    i
  }
  
  ## Return list of methods
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # returns inverse matrix
  i <- x$getInverse()
  #get the inverse if it is already cached
  if(!is.null(i))
  {
    message("Getting cache data")
    return(i)
  }
  
  data<- x$get()
  #compute Inverse
  i <- solve(data)
  
  # set the inverse
  x$setInverse(i)
  
  #return the matrix
  i
}
