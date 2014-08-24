## These functions are to inverse a matrix and cache the result
## When you need to get the inverse of the matrix, check the cached value
## first, if it exists, return. Otherwise, calculated it and return.

## this function return a list with different subfunctions.

makeCacheMatrix <- function(x = matrix()) {
	inm <- NULL
	set <- function( y ){
		x <<- y
		inm <<- NULL
	}
	get <- function() x
	setInverse <- function( inverse ) inm <<- inverse
	getInverse <- function() inm
	list( set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse )
}


## check if cache avaible, if so return it, otherwise, calc

cacheSolve <- function(x, ...) {
	res <- x$getInverse()
	if( !is.null( res ) ){
		message( "Getting cached data" )
		return( res )
	}
	
	data <- x$get()
	res <- solve( data, ... )
	x$setInverse( res )
	res
}
