## This is a script produced for an assignment for R programming, whose goal is to solve matrices while caching their solutions
## makeCacheMatrix creates an object which can be used by the cacheSolve function.
## cacheSolve checks to see if the passed matrix object already has a solution, and if not it calculates one. Otherwise it retrieves the cached solution.

## makeCacheMatrix takes a matrix as its argument and produces a cached matrix object for use by cacheSolve
## it initializes the matrix solution in the cache as a null value
## it contains functions to retrieve the matrix object used to create it, and to retrieve the solution to that object once it exists

makeCacheMatrix <- function(x = matrix()) {
	#initialize the matrix solution as a NULL value
	solvedmatrix <- NULL
	#set the matrix values
	setmatrix <- function(input){
		x <<- input
		solvedmatrix <<- NULL
	}
	#cache the solution
	setsolvedmatrix<-function(solution){
		solvedmatrix <<- solution
	}
	#return the matrix values
	getmatrix <- function(){
		x
	}
	#return the solved matrix
	getsolvedmatrix<-function(){
		solvedmatrix
	}
	list(setmatrix = setmatrix, setsolvedmatrix = setsolvedmatrix, getmatrix = getmatrix, getsolvedmatrix = getsolvedmatrix)
}


## cacheSolve returns the solution for a cacheMatrix object created by makeCacheMatrix.
## it first checks if a solution has already been created, and then creates one if not
## if it does create a solution, it caches that solution

cacheSolve <- function(x, ...) {
    ## get the cached solution
	solvedmatrix <- x$getsolvedmatrix()
	## if a cached solution existed and was not the initial value of NULL, then return it
	if(!is.null(solvedmatrix)){
		message("retrieving cached matrix solution")
		return(solvedmatrix)
	}
	## if no solution existed, load the matrix and solve it.
	data <- x$getmatrix()
	solvedmatrix <- solve(data, ...)
	#cache the solution if you had to make one
	x$setsolvedmatrix(solvedmatrix)
	#return the solved matrix
	solvedmatrix
	
}
