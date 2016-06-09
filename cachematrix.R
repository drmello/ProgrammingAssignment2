## Makes a matrix object that can cache its' inverse
## The inverse of a matrix is a matrix that when multiplied
# by the original matrix equals the identity matrix. Difficult/painful to do above a 2x2 matrix
#by hand



makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
  inv<<-NULL
  }
    get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}

  #the following code computes the inverse of the special 'matrix' created by above code. 
  # if inverse has been calculated beofre, will retrieve from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrx<- x$get()
  inv <- solve(matrx, ...)
  x$setinverse(inv)
  inv
}
