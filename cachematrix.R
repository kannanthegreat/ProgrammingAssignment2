## Caching the inverse of a matrix precludes repeated computation of Inverse of
## matrix. makeCacheMatrix creats a special "matrix" object that can cache 
## its inverse. makeCacheMatrix contains four functions
## 1. Set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y){ 
	x <<- y             ## Used ' <<- ' assign a value to an object 
			    ## different from current enviornment.

	inv <<- NULL

	}
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

} 

## cacheSolve function computes the inverse of of the special "matrix" 
## returned by Cachematrix above.
## If the inverse has already been calculated it skips the computation and 
## gets the result. If not, it computes the inverse and sets the value in the 
## cache via setinverse function. This fuction assumes the invertibility of 
## the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse() 

	if(!is.null(inv)) { 

	message("Getting cached data") 

	return(inv)  

	} 

	data <- x$get() 

	inv <- solve(data) 

	x$setinverse(inv) 

	inv

} 

## Sample run:

## a <- diag(5,3)
## a

## CachedMarix <- makeCacheMatrix(a)
## cacheSolve(CachedMarix)
## b <- diag(2,6)
## b
## CachedMarix <- makeCacheMatrix(b)
## cacheSolve(CachedMarix)     
## Solution on running on R- Studio:
## > a <- diag(5,3)
## > a
##     [,1] [,2] [,3]
## [1,]    5    0    0
## [2,]    0    5    0
## [3,]    0    0    5
## > 
## > CachedMarix <- makeCacheMatrix(a)
## > cacheSolve(CachedMarix)
##     [,1] [,2] [,3]  
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2
## > b <- diag(2,6)
## > b
##    [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]    2    0    0    0    0    0
## [2,]    0    2    0    0    0    0
## [3,]    0    0    2    0    0    0
## [4,]    0    0    0    2    0    0
## [5,]    0    0    0    0    2    0
## [6,]    0    0    0    0    0    2
## > CachedMarix <- makeCacheMatrix(b)
## > cacheSolve(CachedMarix)
##     [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  0.5  0.0  0.0  0.0  0.0  0.0
## [2,]  0.0  0.5  0.0  0.0  0.0  0.0
## [3,]  0.0  0.0  0.5  0.0  0.0  0.0
## [4,]  0.0  0.0  0.0  0.5  0.0  0.0
## [5,]  0.0  0.0  0.0  0.0  0.5  0.0
## [6,]  0.0  0.0  0.0  0.0  0.0  0.5
## > 

