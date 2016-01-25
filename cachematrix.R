
## Matrix inversion is a costly computation and this function is used for caching 
## the inverse of a matrix rather than compute it repeatedly. 


#create variable to store inverse matrix

makeCacheMatrix <- function(x=matrix()){
  im<- NULL
  
  # function for matrix change
  set<- function(y){    
    x<<- y
    im<<- NULL
  }
  # function returns stored matrix
  get <- function()x
  # function for inverse matrix change
  setimatrix<- function(imatrix) im<<- imatrix
  # function returns stored inverse matrix
  getimatrix <- function() im
  list(set=set, get=get, setimatrix=setimatrix, getimatrix=getimatrix)
}

#this function computes inverse matrix returned from makeCacheMatrix

cacheSolve <- function(x,...){
  im<- x$getimatrix()
  
  # if inverse matrix exists return the cache
  if(!is.null(im)){
    message("getting cached inverse matrix")
    return(im)
  }
  #otherwise calculate the inverse matrix
  
  data<- x$get()
  im<- solve(data, ...)
  x$setimatrix(im)
  im
}