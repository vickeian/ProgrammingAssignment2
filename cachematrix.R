## These functions will help to compute and cache the inverse of a matrix
## and help you to retrieve the computed matrix's inverse from memory.
## These functions are also time-savers that prevent the needs to computing the inverse
## of a matrix repetitively while you work on data.

## The first function 'makeCacheMatrix' help creates a matrix that can have its
## inverse cached.

makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## The second function 'cacheSolve' will either call in a previously computed
## inverse of the special matrix, or it will help to compute the inverse of the special
## matrix that you created using the 'makeCacheMatrix' function above.

cacheSolve <- function(x,...){
  inv <- x$getInverse()
  if(!is.null(inv)){
     message("Hang on, getting cached data...")
     return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
