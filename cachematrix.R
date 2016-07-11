## The following pair of functions calculates the inverse of a matrix by caching the inverse of a matrix and retrieving it from the cache.

## The first function, makeCacheMatrix, creates a special "matrix"
## object that can cache its inverse. It contains functions that:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}

## The following function calculates the inverse of the special
## "matrix" created with the function makeCacheMatrix(). However, it
## first checks to see if the inverse has already been calculated. If
## so, it retrieves the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse and sets the
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
