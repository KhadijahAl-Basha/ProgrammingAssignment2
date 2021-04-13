## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special Matrix to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(m){
		x <<- m 
		inv <<- NULL
	}

	getValue <- function() {x}

	setInv<- function(inverse) {
		inv <<- inverse
	}

	getInv <- function() {inv}
	list(set=set, getValue=getValue, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix returned from the makeCacheMatrix. 
## If the inverse was calculated, it retrieves the inverse from the cached data.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$getValue()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}
