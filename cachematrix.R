## Together, these functions take the cache of the inverse of a matrix.

## makeCacheMatrix creates the cache matrix, and it stores functions that may be used
## to retrieve values within the matrix

## cacheSolve uses the solve() function to caculcate the inverse of the matrix. It
## outputs the final product, and it updates the cache matrix.



makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		
		set <- function(y) {
				x <<- y		
				inv <<- NULL
		}
		
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
	
}


cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
		
}
