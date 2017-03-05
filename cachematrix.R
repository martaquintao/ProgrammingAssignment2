## These functions will follow the exact same logic as in the mean example. To use these 
## functions, one must first create a "cache matrix" object (using the function makeCacheMatrix) 
## and only then, use de cacheSolvefunction to make use of the outputed vector of funtions


## makeCacheMatrix: returns "cache matrix" object
## This function will return a "cache matrix" object which will basically consist on a vector of 
## functions whoose environment will contain the inputed matrix and, if it's set, the inverse of 
## the matrix. Included methods:
## 		- set(y): set the matrix value with a new (y inputed) matrix
## 		- get(): get the current matrix values
## 		- setInverse(inv): set the inverted matrix with the inputed (inv) matrix value
## 		- getInverse(): get the current inverted matrix values
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) m <<- inv
	getInverse <- function() m
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## cacheSolve: returns inverted matrix
## To get the inverted matrix, we use the "cache matrix" object we created as an input of this 
## function. This function will use the available methods of the "cache matrix" object to check 
## if an inverted matrix was already calculated in the respective function's environment. If not,
## the function will calculate the inverse and invoque the setInverse function to store it's value.
cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
