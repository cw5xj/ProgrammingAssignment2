## The following pair of functions compute and cache the inverse of a matrix
## so that the inverse can be simply retrieved from the cache without repeated
## computation if the inverse has already been computed before.

## This function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## It returns a lits containing four functions
## 1: "set"-set the value of the matrix
## 2: "get"-get the value of the matrix
## 3: "setInvMatrix"-set the value of the inverse of the matrix
## 4: "getInvMatrix"-get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  InvM<-NULL
  set<-function(y){
    x<<-y
    InvM<<-NULL
  }
  get<-function() x
  setInvMatrix<-function(InvMatrix) InvM<<-InvMatrix
  getInvMatrix<-function() InvM
  list(set=set,get=get,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}


## This function "cacheSolve" computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, simply retriev the inverse from the cache.
## If the inverse has not been calculated, compute it with solve function in R or ginv 
## function in MASS Package, and cache the computed inverse with setInvMatrix defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvM<-x$getInvMatrix()
  if(!is.null(InvM)){
    message("getting cached data")
    return (InvM)
  }
  data<-x$get()
  #use "solve" function to compute inverse of matrix, or use "ginv" function which requires loading MASS package
  InvM<-solve(data,...) 
  x$setInvMatrix(InvM)
  InvM
}
