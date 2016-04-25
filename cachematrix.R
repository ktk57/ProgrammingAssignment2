## The following two functions provide an interface for a "cacheable" matrix
## i.e a matrix whose inverse will be cached
## Finding inverse of a matrix is an expensive operation, hence we can use this cacheable matrix to 
## optimize the use when we are trying to find the inverse of a matrix multiple times.




## makeCacheMatrix() creates a "cacheable" matrix
## it acts as a constructor function which returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	get <- function() x

	getInv <- function() inv

	set <- function(y) {
		x <<- y
	}

	setInv <- function(y) {
		inv <<- y
	}

	list(get=get, getInv=getInv, set=set, setInv=setInv)
}


## cacheSolve is the function equivalent to solve() and returns the inverse
## of matrix, assuming that the matrix is non-singular

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	result <- x$getInv()
	if (!is.null(result)) {
		message("returning cached inverse")
		return(result)
	}

	data <- x$get()
	result <- solve(data, ...)
	x$setInv(result)
	result
}
