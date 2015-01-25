## Below are two functions to create a custom matrix 
## to which you can cache its inverse matrix.

## The makeCacheMatrix() function receives a matrix x as an argument 
## to create and return from it a matrix, with functionality to cache its inverse matrix. 

## This matrix has the get() and set() functions to get and set the internal matrix
## and getsolve() and setsolve() to get and set the inverse of the matrix. 
## It also have the internal variable s, which is the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The cacheSolve function receives as a parameter a personalized x matrix, 
## calls the function to obtain the inverse matrix getsolve (), 
## if it exists returns the inverse matrix s, if not, then call the get () function 
## to get the internal matrix then generates the inverse matrix to solve () function 
## on the data of the internal matrix.

## Then place it in the custom matrix with setsolve () function 
## terminates returning the inverse matrix s.

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
