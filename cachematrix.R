##      There are not too much to say because the example inside the
##      Programming Assignment 2: Lexical Scoping was too clear,
##      I simply rewrite the two functions and after that
##      I test it with a square matrix.

##      The function makeCacheMatrix creates a special "matrix",
##      which is really a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##      The following function calculates the inverse of the special "matrix"
##      created with the above function (using the R solve() function).
##      It first checks to see if the inverse matrix has already been calculated.
##      If so, it gets the inverse matrix from the cache and skips the computation.
##      Otherwise, it calculates the inverse matrix of the data and sets the value of the
##      inverse matrix in the cache via the setsolve function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}