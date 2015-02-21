##this function creates a special matrix, which is a list of functions

makeCacheMatrix <- function(x = matrix()) {
  ##set inverse to null
  inv<-NULL

  ##the set function sets the value of the vector x to a certain vector
  ##sets the inverse to NULL	
  set<-function(y){
  x<<-y
  inv<<-NULL
  }

  ##the get function returns x
  get<-function() x

  ##setmatrix sets inverse to the inverse value
  setmatrix<-function(solve) inv<<- solve

  ##getmatrix returns inverse
  getmatrix<-function() inv

   ##return value of the function makeCacheMatrix
   list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


##This function is calculating the inverse of a matrix
##It checks before calculating if the inverse has already been calculated
##if its already been calculated it returns the value of the inverse which was stored before
##if it hasnt been calculated it calculates the inverse with the solve-function

cacheSolve <- function(x=matrix(), ...) {
    ##checks if inverse is already calculated and cached
    inv<-x$getmatrix()
    ##if its cached return the cached value	
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    ##if not calculated, get the matrix
    matrix<-x$get()
    ##calculate the inverse of a matrix
    inv<-solve(matrix, ...)
    ##store the inverse in cache
    x$setmatrix(inv)
    ##return inverse
    inv
}