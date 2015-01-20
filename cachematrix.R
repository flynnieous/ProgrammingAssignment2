## makeCacheMatrix builds a list containing  methods to get and
## set the matrix and methods the cached inversion 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL;
    
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setSolve <- function(solve) s <<- solve
    
    getSolve <- function() s

    list(set = set, 
         get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}


## cacheSolve returns the cached inversion of the matrix.
## If the inversion has not been calculated (the value is NULL)
## it calculates, caches, and returns the inversion.

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if (! is.null(s)) {
        print("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...) 
    x$setSolve(s)
    s
}
