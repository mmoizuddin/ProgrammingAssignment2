## The purpose of two functions defined below "makeCacheMatrix" and "cacheSolve" 
## is to cache potentially time-consuming computations rather than compute it repeatedly,
## such as matrix inversion
## e.g.
## > a <- makeCacheMatrix(matrix(1:4,2))        -- matrix to be inversed
## > b <- cacheSolve(a)                         -- if inverse for the matrix is loaded in memory
##                                                 then return the message otherwise compute it
## > b                                          -- result inverse matrix
#######################################################################################
##
## This function creates a matrix that can cache its inverse.
## The function returns a list of funtions, cached in memory:
## "set" <-- set the value of the matrix
## "get" <-- get the value of the matrix
## "setsolve" <-- set the value of the inverse
## "getsolve" <-- get the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}

#######################################################################################
## Write a short comment describing this function
##
## This function checks if the inverse is aleady been calculated for the same matrix 
## it returns the inverse from memory. Otherwise, sets the value of the inverse using "setsolve" function
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
