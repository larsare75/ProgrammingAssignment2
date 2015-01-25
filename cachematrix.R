## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#This function does not do any calculation of the matrix but is used as a container to 
  #maintain its values by using the inbuilt functions
  inv<-NULL
  ##Initially sets the values of the matrix within the vector
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ##Returns the contents of the matrix itself
  get <- function() x
  ##sets inv to a matrix which is inverse of the original matrix
  setinv<-function(inverse) inv <<-inverse
  ##returns the inverse matrix of the original matrix
  getinv<-function() inv
  ##returns list containing the offered functions
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  ## First: Trying to retrieve the cached value
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)    
  }
  ## If not already cached we need to do the inversion, then cache it and then return it
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
