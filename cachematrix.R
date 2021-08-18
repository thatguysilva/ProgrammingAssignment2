## program submitted as a requirement for the R programming course from John Hopkins University at Coursera
## we write two functions to cache the inverse of a matrix


## first create the cache object

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() {x}
	setInverse <- function(inverse){inv <<- inverse}
	getInverse <- function() {inv}
	list(set = set,
             get = get,
	     setInverse = setInverse,
             getInverse = getInverse)
}


## then solve, store and return inverse of cached matrix, or only return if already stored


cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()              ## get matrix to inverse
	inv <- solve(data, ...)      ## inverse the matrix
	x$setInverse(inv)            ## store
	inv                          ## return inverse
}

## program can be tested with
## testmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## cacheSolve(testmatrix)

## concluded 26/07

