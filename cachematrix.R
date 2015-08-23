## The given pair of functions allows to cache the inverse of a matrix.
## More details can be found in comments to the functions themselves.
##
## Here is a sample usage:
##
## > A = matrix(runif(1000*1000), nrow=1000, ncol=1000)
## > objA = makeCacheMatrix(A)
##
## A is a 1000x1000 matrix containing random numbers and objA is an
## object that contains this matrix and its inverse (yet to be computed).
##
## Now cacheSolve(A) can be used to obtain the inverse of A. During the first
## call the inverse will actually be computed. In all subsequent calls the
## inverse will not be recomputed and will be taken from cache instead.
##
## >system.time(cacheSolve(objA))
## user      system       elapsed 
## 0.95        0.00          0.95 
##
## Here the inverse was computed and it took almost 1 second.
##
## > system.time(cacheSolve(objA))
## user      system       elapsed 
##    0           0             0
##
## Here the inverse was taken from cache and it happened almost instantly.




## The function makeCacheMatrix creates a special kind of matrix-like object
## that contains information about a matrix and its inverse.
##
## Interaction with the returned object can be performed via 4 methods:
## 1) setMatrix(y) -- sets the matrix stored to "y".
## 2) getMatrix() -- returns the matrix stored.
## 3) setInverse(inverse) -- sets the inverse of the matrix stored to "inverse".
## 4) getInverse() -- returns the inverse of the matrix stored.
##
## The getInverse() function will return NULL if no inverse was set via
## setInverse() after the object's creation or the last call to setMatrix().

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setMatrix <- function(y) {
		x <<- y
		inv <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(setMatrix = setMatrix,
	     getMatrix = getMatrix,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## In function cacheSolve "x" should be an object created by makeCacheMatrix().
## The function will return the inverse of the matrix stored within x.
## This inverse will be computed if it has not yet been computed (after
## the object's creation or the last call to x.setMatrix()).
## Otherwise, the inverse will not be computed and the cached value
## will be returned instead.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
		return(inv)
	}
	data <- x$getMatrix()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
