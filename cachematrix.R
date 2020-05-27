## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initiliaze the default value 
  m<- NULL 
  
  #Set the matrix
  set<- function(matrix){
        x<<-matrix
        m<<-NULL
  }
  
  #Get the matrix
  get<- function(){
    x
  }
  
  #Set the inverse of matrix
  setInverse<- function(inverse){
    m<<- inverse
  }
  
  #Get the inverse of matrix
  getInverse<- function(){
    m
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #Return a matrix that is the inverse of x      
  m<- x$getInverse()
  
  #Just for inverse value availability
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  #Get the inverse value
  data<- x$get()
  
  #Compute the inverse
  m<- solve(data)%%data
  
  #Set the inverse value to the object
  x$setInverse(m)
  
  #Return the inverse value
  m
}
