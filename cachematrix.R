## Put comments here that give an overall description of what your
## functions do

## This function creates the makeCacheMatrix object with by initializing global 
##variable and creating get, getinverse and setinverse function

makeCacheMatrix <- function(x = matrix()) 
{ 
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_mat) m <<- inv_mat
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
} 


## This function creates the cacheSolve function which will retrive the inverse of the 
##non singular matrix whose determinate in not zero. If matrix will exist then it will fetch it from cache

cacheSolve <- function(x, ...) 
{ 
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getinverse()
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
} 

##Example with Test Scenarios and expected output. Singular matrix is inversed and 
## retrieved from Cache second time

##myinput <- matrix(c(-1,3/4,1/2,-1/4),2,2)

##matrixObject <- makeCacheMatrix(myinput)

##matrixObject$get()
##[,1]  [,2]
##[1,] -1.00  0.50
##[2,]  0.75 -0.25

##matrixObject$getinverse()
##NULL

##cacheSolve(matrixObject)
##[,1] [,2]
##[1,]    2    4
##[2,]    6    8

##cacheSolve(matrixObject)
##getting cached data
##[,1] [,2]
##[1,]    2    4
##[2,]    6    8

##matrixObject$getinverse()
##[,1] [,2]
##[1,]    2    4
##[2,]    6    8
