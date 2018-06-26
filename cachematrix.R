## The first function creates a matrix that is assumed to be invertible and creates a list containing functions to
## set/get the matrix and set/get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x<<-y
    inv <<- NULL
  }
  ## create list of functions
  get<-function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This second function returns the inverse of the matrix by either solving or getting the cached inverse

cacheSolve <- function(x, ...) {
  ## cached inverse
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  ## otherwise solve the inverse
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


testmatrix <- matrix(1:4,2,2)
a<- makeCacheMatrix(testmatrix)
cacheSolve(a)