
makeCacheMatrix <- function(m = matrix()) {
		## This function returns a list containing the 'getter' and 'setter' functions
		## on the invert of the matrix 'm' given as inputs
		## What is fun :-) is that the inverted matrix is stored as a 'kind-of' data member 
		## of the object in the returned list (sorry for C++ terminology :-) named 'invert' 
		## to avoid recomputing it in case it was already computed/inverted before
        	
		## Assign invert (in the local function scope) to NULL
		invert <- NULL
        
		## 'Set' function that copies its input into 'm', a kind-of 'data member' 
		## And resets its inverted copy
		set <- function(x) {
                m <<- x
                invert <<- NULL
        	}
		
		## 'Get function that returns the 'm' matrix
        	get <- function() {m}

		## Set and Get function for the inverted matrix in cache
        	setinvert <- function(inv) {invert <<- inv}
        	getinvert <- function() {invert}
        
		## Return a list with 4 labels: 
		## - set, get (for the matrix)
		## - setinvert, getinvert (for its invert)
		list(	set = set, 
			get = get,
            	setinvert = setinvert,
             	getinvert = getinvert)
}


## The cacheSolve function calculates the invert of the special "matrix" in the lis created 
## with the above function. 
## However, it first checks to see if the invert has already been computed. 
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it performs the matrix invertion and sets the value of the invert 
## in the cache via the setinvert function.

cacheSolve <- function(x, ...) {
        	## Get the 'invert' member of the matrix (and check if it was already computed)
		inv <- x$getinvert()

		## If 'invert' is not null (i.e. we have already computed and stored in memory cache
		## the invertion of the matrix 'x' passed as input
		## Then simply return the getinvert of the input matrix
        	if(!is.null(inv)) {
                message("Getting cached date for matrix invertion")
                return(inv)
        	}
        	
		## Else compute the invertion of the matrix (matrix accessible in the 'get' of x)
		## By calling the solve function
		matrix <- x$get()
        	inv <- solve(matrix, ...)
        
		## Store the inverted matrix in cache in the 'setinvert' part of the list
		x$setinvert(inv)
        
		## And end by returning the inverted matrix :-)
		inv
}