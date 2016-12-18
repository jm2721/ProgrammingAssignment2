## Put comments here that give an overall description of what your
## functions do

## returns a list of four functions: set, get, setInverse, getInverse, that can be used
## to manipulate the underlying matrix, and store/return the inverse.

makeCacheMatrix <- function(x = matrix()) {
	mInverse <- NULL
	set <- function(y) {
            x <<- y
            mInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(pInverse) mInverse <<- pInverse
	getInverse <- function() mInverse

	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the cached inverse, if present. Otherwise it computes it and caches it.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		message("found a cached inverse. returning it")
		return(inverse)
	}
	message("didn't find a cached inverse. computing it")
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setInverse(inverse)
	inverse
}
