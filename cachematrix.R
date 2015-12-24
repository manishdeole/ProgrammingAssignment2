## We have a set of functions ('makeCacheMatrix' and 'cacheSolve') defined here to read the matrix,
## compute its inverse and store it in cache. If the matrix remains unchanged and the inverse is required
## multiple times, the 'cacheSolve' function fetches the inverse from the cache thus avoiding repeated
## computation and saving time / resources.
## An assumption for these functions (for this assignment) is that the supplied matrix is always invertible.

## The 'makeCahceMatrix' function creates a special "matrix" that is a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of inverse of matrix and
## get the value of inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get,
	     setmatrix=setmatrix,
	     getmatrix=getmatrix)
}


## The 'cacheSolve' function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}
