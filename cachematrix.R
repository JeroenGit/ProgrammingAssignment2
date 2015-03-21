## 
## Cache Matrix offers functions that enables optimal calculation
## of matrix inversion. Once calculated the inverse value is cached
## so that the next time - if unchanged - the value will not have to be
## calculated again.
##
 
## 
## makeCacheMatrix 
## creates a list that define get/set functions for both
## the maatrix value and the inverse value
##
makeCacheMatrix <- function(x = matrix()) { 
	# the cache to store the solved matrix in
	cache <- NULL
	
	# the function to set a new matrix value and clear the cache
	set <- function(y) {
		x <<- y
		cache <<- NULL
	}
	
	# the function to get a matrix value
	get <- function(y) {
		x
	}
		
	# the function to set the matrix inverse value
	setInverse <- function(inverse) {
		cache <<- inverse
	}
	
	# the function to get the matrix inverse value
	getInverse <- function() {
		cache
	}
	
	# return the cacheMatrix making object as a list
	list(set = set, get = get, 
		 setInverse = setInverse, getInverse = getInverse)
} 
 
## 
## cacheSove
## returns the inverse of a given matrix
## by checking if it has been calculated before, if not it will
## calculate it now and store it for future use
cacheSolve <- function(x, ...) {
	# first see if the value has been calculated before
	inverse <- x$getInverse()
	
	if (!is.null(inverse)) {
		# found previous calculated value 
		message("getting cached data")
		return(inverse)
	}
	
	# previous calculated value not found so calculate it
	inverse <- solve(x$get(), ...)
	
	# cache it for use next time
	x$setInverse(inverse)
	
	# return the inverse value
	inverse
} 
