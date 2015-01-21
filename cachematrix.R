## The functions below cache the inverse of a matrix rather than compute it repeatedly.
## See each function for more details.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# Creates a special "matrix" object that can cache 
	# its inverse.
	# x: is a matrix object
	# Returns: a list that makes up the special "matrix" 
	# object with elements "get" which retrieves the 
	# matrix, "setinv" which the assigns the object its 
	# inverse, and "getinv" which retrieves the inverse
	# of the matrix.
        m <- NULL 
        get <- function() x  
        setinv <- function(inv) m <<- inv  
        getinv <- function() m   
        list(get = get, setinv = setinv,
             getinv = getinv )	
}


## This function computes the inverse of the special "matrix" object
## returned by makeCacheMatrix above. If the inverse has already 
## been computed (and the matrix has not changed), then  
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Retrieves or computes the inverse of x.
	# x: is a matrix object
	# Returns: the inverse of x 
		
		m <- x$getinv()  # retrieve the inverse if it's been 
						 # computed before
        if(!is.null(m)) {  
                message("getting cached matrix inverse")
                return(m) # return the cached inverse
        }
        data <- x$get()   # retrieve the matrix
        m <- solve(data, ...)  # compute the inverse of the matrix
        x$setinv(m)   # assign the inverse to the special matrix 
					  #bject for future usage
        m # returns the inverse
}
