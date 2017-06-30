## ProgrammingAssignment2
## functions for creating a special "matrix" that will cache its inverse

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix using solve()
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## clear the holding variables
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # define the required get, set functions
        
        get <- function() x
        setInverse <- function(x) m <<- x
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if m is null, do the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}
