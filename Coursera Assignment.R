makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function(){x}
  setMatrixInverse <- function(inv) {inverse <<- inv}
  getMatrixInverse <- function(){inverse}
  list(set = set, get = get, setInverse = setMatrixInverse, getInverse = getMatrixInverse)
}
cacheSolve <- function(x, ...){
  c <- x$getInverse()
  if(!is.null(c)){
    message("getting cached data")
    return(c)
  }
  matrix <- x$get()
  c <- solve(matrix, ...)
  x$setInverse(c)
  c
}