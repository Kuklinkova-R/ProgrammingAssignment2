
#creating a special matrix that caches its inverse version
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse #set the inverse
getinverse<- function() inv          #get the inverse
list(set = set,
     get = get,
     setinverse= setinverse,
     getinverse = getinverse)
}

#computing the inverse if the special matrix
cacheSolve <- function(x, ...) {
#checking whether the inverse has already been calculated (and the matrix has not changed),
  # if yes, then the cacheSolve retrieves the inverse from the cache

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if not, the inverse is calculated and returned
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
