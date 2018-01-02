## Peforms caching operations which saves computation time

## Function used to create a special object that stores a matrix to set and get, the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
        
	get <- function() x
        
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
        
	list(set = set, get = get,
		setinverse = setinverse,
	getinverse = getinverse)
}


## Function used to compute the inverse of the matrix returned by the makeCacheMatrix above.
## If the inverse is calculated and matrix has not change, the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
       	if(!is.null(i)) {
                message("getting cached data")
		return(i)
        }
        
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i

}
