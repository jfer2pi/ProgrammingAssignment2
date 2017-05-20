## These functions are meant to work together to calculate and store the inverse of
## a matrix.  The inverse of the matrix input is stored in an environment to reduce
## the burden of calculation in the future, if the inverse already exists, then it is 
## simply obtained from the environment, if it is not, it is calculated.


## The makeCacheMatrix function has a matrix input x and outputs a set of functions
## as a list.  These functions are meant to be input into the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # Defines the get function that outputs the matrix input
    setinv <- function(solve) m <<- solve # Set matrix inverse into cache
    getinv <- function() m # Retrieves matrix inverse from cache
    list(set = set, get = get, setinv = setinv, getinv = getinv) # Creates a list of
    #functions to be entered into cacheSolve
}


## The cacheSolve function takes the output of makeCacheMatrix and evaluates
## whether the inverse has been calculated or not, if yes, it retrieves the value
## without a calculation, if it does not, it calculates the matrix inverse and stores
## it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv() # Retrieves inverse of matrix x
    if(!is.null(m)) { # Evaluates if inverse of matrix x exists and displays if yes
        message("Obtaining cached data")
        return(m)
    }
    data <- x$get() # If it does not exist, the function retrieves matrix 
    m <- solve(data, ...) # Solves for its matrix inverse
    x$setinv(m) # Stores it in cache
    m # Prints to console
    
}
