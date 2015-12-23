## Put comments here that give an overall description of what your
##   Matrix inversion is usually a costly computation and there may be some benefit
##   to caching the inverse of a matrix rather than compute it repeatedly. 
##   It includes 2 functions:
##     1. makeCacheMatrix: This function creates a special "matrix" object that can
##        cache its inverse.
##     2. cacheSolve: This function computes the inverse of the special "matrix" 
##        returned by makeCacheMatrix above. If the inverse has already been calculated
##        (and the matrix has not changed), then the cachesolve should retrieve the 
##        inverse from the cache.

## Write a short comment describing this function
##   creates a special "matrix" which is a list containing functions to set/get the value
##   of matrix and set/get the inverse matrix 


makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL
	set <- function(y){
		x <<- y
		inverse_x <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseM) inverse_x <<- inverseM 
	getInverse <- function() inverse_x
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##   calculate the inverse, but it will check if a cached value exists at first. 
cacheSolve <- function(x, ...) {
	inverse_x <- x$getInverse()
	if (!is.null(inverse_x)) {
		message("get cached data")
		return(inverse_x)
	}
	data <- x$get()
	inverse_x <- solve(data,...)
	x$setInverse(inverse_x)
	inverse_x
}