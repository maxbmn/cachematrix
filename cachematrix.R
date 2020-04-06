#Programming Assignemt 2: Lexical Scoping
## With these functions makeCacheMatrix() and cacheSolve() we are able to compute
## inversive matrix.

#Beginning with the makeCacheMatrix function...
#Initalize all functions to cache the Matrix.

makeCacheMatrix<-function(x=matrix())
{
  inv<-NULL
  set<-function(y)
  {    x<<-y
  inv<<- NULL}
  
  get<- function() x
  setInvers <-function() inv<<-solve(x)
  getInvers <- function() inv
  
  list(set= set,
       get= get,
       setInvers= setInvers,
       getInvers= getInvers)
}


#After this... creating the function to computes the inverse of the special "matrix".
#If the matrix is already cached we get the text "getting cached data..." and return the data
#If the matrix needs to compute.... this will happen 
#and the new inversive matrix will be returned as inv2!


cacheSolve<-function(x,...){
  inv<-x$getInvers()
  if(!is.null(inv))
  {
    message("getting cached data...")
    return(inv)
  }
  data<-x$get()
  inv2<- solve(data,...)
  inv2
}
