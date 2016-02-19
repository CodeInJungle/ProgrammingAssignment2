## makeCacheMatrix function act as a matrix cache. It takes a matrix as argument 
## and returns a list of functions to set,get matrix and its 
# inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x
	
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	
	list (set = set, get = get,
		  setinverse = setinverse,
		  getinverse = getinverse)

}


## cacheSolve computes the inverse of a matrix and uses cache to speed operations
## It takes makeCacheMatrix function and data as argument
## and returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	m
}

