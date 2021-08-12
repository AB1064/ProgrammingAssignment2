## Put comments here that give an overall description of what your
## functions do
## These functions capture the inverse of user submitted matrix and checks whether
## the inverse has already been computed and if yes then it extracts it from the
## cache and displays the output to the user or else it computes the inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  ## this function catches the inverse of the user inputted matrix
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)
    inv<<-inverse
  getinverse<-function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## this function calculates the inverse of the matrix created in above function.
  ## inverse will be pulled from cache by cacheSolve function.
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    message("fetching cached stuff")
    return(inv)
  }
  mati<-x$get()
  inv<-solve(mati,...)
  x$setinverse(inv)
  inv
}
