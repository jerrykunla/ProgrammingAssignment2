#These two functions are used to get the inverse of a matrix. 
#Once the inverse of a matrix calculated, it will be stored in the cache, 
#the next time you need it, you just get it from the cache and no need to calculate again


##This function calculates the inverse of a matrix, and store in cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<-NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function is to get the inverse matrix from the cache if it's not NULL. 
## If it is, then calculate the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}
