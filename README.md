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
![image](https://user-images.githubusercontent.com/87285373/125221190-bb3b9b80-e27c-11eb-941f-0c035f8d44ab.png)
