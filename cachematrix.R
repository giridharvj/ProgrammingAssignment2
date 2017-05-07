##Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
##Week 3: Caching the Inverse of a Matrix

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inv<-NULL
  set<-function(y){
    x<<-y
    x_inv<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) x_inv<<- solve
  getmatrix<-function() x_inv
  list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

##cacheSolve function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  x_inv<-x$getmatrix()
  if(!is.null(x_inv)){
    message("getting cached data")
    return(x_inv)
  }
  matrix<-x$get()
  x_inv<-solve(matrix, ...)
  x$setmatrix(x_inv)
  x_inv
}
