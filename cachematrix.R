## Defines an object class (makeCacheMatrix) and a function that
## returns the matrix inverse 


## Initializes an object that stores a matrix and its inverse.
## Has functions for getting and setting values.

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv<<-inverse
	getInverse <- function() inv
	
	list(set=set, get=get, setInverse=setInverse, 							getInverse=getInverse)
}


## Retreives the inverse of a matrix stored as 'makeCacheMatrix'.
## If the value has not been previously stored, the function
## calculates and stores the value.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		return(inv)
	}
	data<-x$get()
	inv<-solve(data, ...)
	x$setInverse(inv)
	inv
}
