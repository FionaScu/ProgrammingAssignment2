## The functions "makeCacheMatrix" and "cacheSolve" work together to cache the inverse of a square matrix, 
## to reduce the computing time on repeated inversions of matricies

## Function "makeCacheMatrix" creates a special matrix that can cache it's inverse and 
## saves it in the parent environment of the function for future use.

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


## function "cacheSolve" calculates the inverse of the special matix created in "makeCacheMatrix", 
## It checks if the inverse has already been cached and if the matrix has not been changed 
## "cacheSolve" returns the cached matrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m       ## Returns a matrix that is the inverse of 'x'
}
