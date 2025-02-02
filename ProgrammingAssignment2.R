##makeCacheMatrix function checks if it has been calculated. It has a list to set, get, setInverse, and getInverse  
makeCacheMatrix <- function(t = matrix()){
  inv <- NULL
  set <- function(f){
    t <<- f
    inv <<- NULL
  }
  get <- function() {t}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve computes the inverse of the matrix returned by makeCacheMatrix:

cacheSolve <- function(x, ...) {
  inv <- t$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  mat <- t$get()
  inv <- solve(mat, ...)
  t$setInverse(inv)
}



