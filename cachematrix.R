## 'makeCacheMatrix' and 'cacheSolve' functions compute, cache
## and return the inverse of a matrix, reducing computation time
## when requesting repeatedly the inverse of the same matrix 


## 'makeCacheMatrix' creates a list of functions that set the matrix 'x',
## get the stored matrix 'x', set the inverse matrix 'i' and get the stored 
## inverse matrix 'i' from cache

makeCacheMatrix <- function(x=matrix()) {
    i <- NULL                                       # Initializes the resulting inverse matrix 'i' as NULL
    set <- function(y) {                            # Modifies matrix 'x'
        x <<- y
        i <<- NULL
    }
    get <- function() x                             # Returns matrix 'x'
    setinverse <- function(inverse) i <<- inverse   # Modifies the inverse matrix 'i'
    getinverse <- function() i                      # Returns the stored inverse matrix 'i' from cache
    list(set = set, get = get,  
         setinverse = setinverse,
         getinverse = getinverse)                   # List with the four functions
}


## "cacheSolve" returns a matrix 'i' that is the inverse of matrix 'x'
## When previously computed, 'i' is returned from the cache memory instead
## performing a new time-consuming computation

cacheSolve <- function(x,...) {
    i <- x$getinverse()                                 # Get the stored inverse matrix 'i' from the cache
    if(!is.null(i)) {                                   # If already computed and stored then 'i' is returned
        message("Getting inverse matrix from cache")    # from the cache
        return(i)
    }
    data <- x$get()                                     # If not, then matrix 'x' is recovered from the cache 
    i <- solve(data, ...)                               # and its inverse matrix 'i' is calculated, stored 
    x$setinverse(i)                                     # in the cache and returned
    i
}
