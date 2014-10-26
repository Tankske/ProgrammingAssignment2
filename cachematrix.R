    ## Created by YL
	## Created on 2014_10_26
	## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).

	## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
	## Creates functions to store&retrieve matrix.
	## Creates functions to store&retrieve inverse of the matrix.
	
makeCacheMatrix <- function(m = matrix()) {
		##Make sure the variable used to cache the inverse is empty
		i <- NULL
		##Set function that sets matrix data equal to m, set inverse matrix to null
        set <- function(y) {
                m <<- y
                i <<- NULL
        
		##Get function to retrieve data
        get <- function() m
		##Set function to cache inversed matrix
        setinverse <- function(solve){
			i <<- solve
		}
		##Get inversed matrix related to this data
        getinverse <- function() {
			i
		}
		##Return list with function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

	## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
		##Try to find the cached inversed matrix for m
        i <- m$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(i)
        }
		##As no cached inversed matrix is found, get the data
        data <- m$get()
		##Calculate the inverse
        i <- solve(data, ...)
		##Cache the inversed matrix
        m$setinverse(i)
		##Return the result
        i
}

