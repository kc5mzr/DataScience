makeCachedMatrix <- function(neo = matrix()) {
  # Cache the inverse of the passed matrix.
  if (dim(neo)[1] != dim(neo)[2]) {
    message("Matrix must be a Square Matrix")
    return()
  }
  trinity<-NULL
  set<-function(morpheus){
  neo<<-morpheus
  trinity<<-NULL
}
get<-function()  neo
setmatrix<-function(solve) trinity<<- solve
getmatrix<-function() trinity
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheMatrixSolve <- function(neo=matrix(), ...) {
  #  Get the matrix from Cache, if the passed in Matrix is null.
    morpheus<-neo$getmatrix()
    if(!is.null(morpheus)){
      message("getting cached Matrix")
      return(morpheus)
    }
    oracle<-neo$get()
    morpheus<-solve(oracle, ...)
    neo$setmatrix(morpheus)
    morpheus
}