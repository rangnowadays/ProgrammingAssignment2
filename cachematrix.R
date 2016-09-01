# This code is for Inverse of Matrix: R Programming Assignment 2
# Read.md consists of the Problem statement and instructions for this assignment

# Two functions will be created here.
# 1--> makeCacheMatrix() and 2--> cacheSolve()

# makeCacheMatrix function is used to keep the matrix and the cached value of its inverse
# This function need 4 sub-function which are...
# setMetrix - Set value of the matrix
# getMetrix - Get the value of the matrix
# setInverse - Set the value of inverse of the matrix
# getInverse - Get the value of inverse of the matrix

# cacheSolve function will calculates the inverse of the matrix created by makeCacheMatrix and returns the output

# Solution: To give a sample we consider a 2X2 matrix 
# Giving that V<- matrix(c(2, 3, 4,5), c(2, 2))
# assigning MM <- makeCacheMatrix(V)
# cacheSolve(MM) this will return the solved matrix below  
#     [,1] [,2]
# [1,]   -2.5  2
# [2,]    1.5 -1

makeCacheMatrix <- function(x = matrix()) {
        
        # creating a variable to hold cached value or NULL

        cache <- NULL
        
        # store a matrix
        setMM <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # returns the stored matrix

        getMM <- function() { x }

        setInv <- function(b) {  cache <<- b   }

        # retrieve the cached value
        
	getInv <- function() { cache }
        
        list(
		setMM = setM,
		getMM = getMM,
		setInverse = setInverse,
		getInverse = getInverse
	    )
}


cacheSolve <- function(x, ...) {

        cache <- x$getInv()

        if(!is.null(cache)) {
                message("get cached data")
                return(cache)
        }

        d <- x$getM()
        cache <- solve(d, ...)
        x$setInv(cache)
        cache
}
