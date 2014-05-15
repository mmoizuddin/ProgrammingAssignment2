## The purpose of two functions defined below "makeCacheMatrix" and "cacheSolve" 
## is to cache potentially time-consuming computations rather than compute it repeatedly,
## such as matrix inversion
## e.g.
## > a <- makeCacheMatrix(matrix(1:4,2))  -- matrix to be inversed
## > b <- cacheSolve(a) -- if inverse for the matrix is loaded in memory
##                         then return the message otherwise compute it
## > b -- result inverse matrix
#######################################################################################
##
## This function creates a special "matrix" object that can cache its inverse.
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
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
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
