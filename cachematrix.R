## These functions together allow us to avoid taxing computations by caching the results of the initial
## matrix inversion and reusing when required. 


## The function below creates a special vector, captures the input matrix and sets up the default methods to handle
## the input matrix and its inverse

#input here must be a squared matrix than can be successful inverted
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  	set <- function(y) {
    		x <<- y
    		m <<- NULL
  	}
  	get <- function() x
  	setinv <- function(inv) m <<- inv
  	getinv <- function() m
  	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function reads the input matrix and checks if the inverse has been previously calculated. If so, 
## it returns the cached value. If not, it computes the inverse.

cacheSolve <- function(x, ...) {
	# get input matrix m
	m <- x$getinv()
	# check if the inverse has already been computed. If yes, return cached matrix and tell me so!
  	if(!is.null(m)) {
    		message("We already did that! Getting cached data")
    	return(m)
  	}
	# if not, compute inverse and return it
  	data <- x$get()
  	m <- solve(data, ...)
  	x$setinv(m)
  	m
}        

