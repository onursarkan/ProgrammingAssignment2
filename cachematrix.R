## Coursera R Programming Course
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD

## Programming Assignment 2: Lexical Scoping

## By this assignment, new Caching mechanism is developed for matrix inverse operations.
## This R code contains 2 main functions: makeCacheMatrix, cacheSolve


## makeCacheMatrix: By this function, In-Memory caching object(list with getter and setter) is produced for input matrix. 
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize cache with null value
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## This function return the input matrix:
  get <- function() x
  
  ## This is setter function for caching mechanism:
  setsolve <- function(solve) s <<- solve
  
  ## This is getter function for caching mechanism:
  getsolve <- function() s
  
  ## This is complex output data type with caching capability 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
  
## cacheSolve: This function produce inverse matrix for previous function's outputs.
## If there is a cached result, it returns from cache.
## If there is no cached result, it calculate the inverse matrix, also it caches the result, and than returns the results.
cacheSolve <- function(x, ...) {
  ## It checks the cached value:
  s <- x$getsolve()
  
  ## If there is a cached value, returns this inverse matrix.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## If there is no cached inverse matrix, it takes the input matrix:
  data <- x$get()
  
  ## It produce inverse matrix for input matrix:
  s <- solve(data, ...)
  
  ## Set the cache with calculated inverse matrix:
  x$setsolve(s)
  
  ## It returns newly calculated inverse matrix:
  s
}
