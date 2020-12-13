## Goal: Create a special, invertible matrix and then create a function to return cached inverse values

## This function creates a special matrix that can have it's inverse taken

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x <<- y
   inv <<-NULL
  }
  get <-function ()x
  setinv <-function() inv <<- solve(x)
  getinv<- function() inv
  list(set = set, get = get, setinv=setinv, getinv = getinv)

  } 
  


#cacheSolve: this function returns the inverse cached from the funciton above.
##if the inverse has already been calculated, it will return those values from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  return(inv)
       
}

