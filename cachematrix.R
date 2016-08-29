## The two functions provide th ability to cache an inverse of a matrix and computes it. 

## The makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y  
		m <<- NULL 
	}

	get <- function() x
	setInverse <- function(solve) m<<- solve
	getInverse <- function() m 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## This function computes the inverse od the special matrix object returned by the makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()                 
	if(!is.null(m)){                    
		message("hold tight...retrieving cached data")     
		return(m)                          
	}
		
	data <- x$get()                     
	m <- solve(data, ...)               
	x$setInverse(m) 
}
