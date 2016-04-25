## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
