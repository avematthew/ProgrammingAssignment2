## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	solvedmatrix <- NULL
	setmatrix <- function(input){
		x <<- input
		solvedmatrix <<- NULL
	}
	setsolvedmatrix<-function(solution){
		solvedmatrix <<- solution
	}
	getmatrix <- function(){
		x
	}
	getsolvedmatrix<-function(){
		solvedmatrix
	}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	solvedmatrix <- x$getsolvedmatrix()
	if(!is.null(solvedmatrix)){
		message("retrieving cached matrix solution")
		return(solvedmatrix)
	}
	data <- x$getmatrix()
	solvedmatrix <- solve(data, ...)
	x$setsolvedmatrix(solvedmatrix)
	solvedmatrix
	
}
