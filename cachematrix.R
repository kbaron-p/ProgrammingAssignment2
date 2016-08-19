## Those two functions are supposed to allow to compute the inverse of the matrix and cache it
## so that when next time cache is called it will either return the cached version or if it's empty will compute
## the inverse

## This function will prepare the internal functions that will set the matrix and get it
## as well as set its inverse and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This is function that checks if there is cached version of inverse and returns it,
## but when it's not there it computes and sets it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
}
