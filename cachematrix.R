## The first function will create a special "matrix" object that can cache
## it's inverse. The second function will compute the inverse of this matrix
## from the first function. If the inverse has already been calculated then
## it will be retrieved from cache instead.

## This function creates a special "matrix" defining functions within the
## parent function environment

makeCacheMatrix <- function(m = matrix()) {

	inv <- NULL ## Store the inverse value
	
	## Defining the set function and assigning variables in set environment
	set <- function(y) {
		m <<- y
		inv <<- NULL
	}

	## Defining get function to return m
	get <- function() m

	## Defining setinv function using the 'solve' function.
	## Assumption is that the passed matrix will be invertible.
	setinv <- function(solve) inv <<- solve 

	## Defining getinv function and creating list of functions
	getinv <- function(solve) inv
	list(set = set, get = get,
		setinv = setinv
		getinv = getinv)
}

## This function will check for the inverse matrix in cache. If available
## it will be retrieved, else it will be calculated and stored in cache.

cacheSolve <- function(x, ...) {

	# Logical test to retrieve inverse from cache if available
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}

	# If inverse matrix not available in cache then it is calculated
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}



