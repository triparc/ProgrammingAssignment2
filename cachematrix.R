## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## This is done by a pair of functions: 'makeCacheMatrix' and 'cacheSolve'

#################################################################################### 
##                          ~ makeCacheMatrix ~                                   ##
##                                                                                ##
## This function creates a special "matrix" object that can catch it's inverse.   ##
## It consists of a list of 4 function calls:                                     ##
## 1. set the value of the matrix                                                 ##
## 2. get the value of the matrix                                                 ##
## 3. setinverse - set the value of the inverse matrix                            ##  
## 4. getinverse - get the value of the inverse matrix                            ##                                                                            ##
##                                                                                ## 
####################################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#################################################################################### 
##                               ~ cacheSolve ~                                   ##
##                                                                                ##
## This function computes the inverse of the special "matrix" returned by         ##    
## makeCacheMatrix function                                                       ##
## If the inverse has already been calculated (and the matrix has not changed),   ##
## then the cachesolve retrieves the inverse from the cache. Otherswise,  it      ##
## computes the inverse of the input matrix by using 'solve' function from R.     ##
##                                                                                ##
## Assumption: The input matrix used is invertible.                               ##  
##                                                                                ##   
####################################################################################

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvers(m)
  m
}

