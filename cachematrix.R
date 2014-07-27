## There are two functions here -- makeCacheMatrix() and cacheSolve()
##. At a high level, the work together to cache the inverse of matrices so that we can save repeated calls to solve(). 
## Because computing inverse is an expensive operation this will save CPU at the cost of some memory 

## makeCahce() wraps a matrix into a data structure that also stores its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    m <-NULL
    
    ## setter for the wrapped object. If we set the matrix, we need to forget the old inverse
    set <- function(y) {
        ## "superassignment" searches upward in the lexical scope. 
        x <<- y
        m <<- NULL
    }
    
    ## getter for the wrapped matrix 
    get <- function() x
    
    ## sets the inverse
    setinverse <- function(inverse) m <<-inverse
    
    ## returns the stored inverse
    getinverse <- function() m
    
    ## return value for makeCache -- a list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns cached matrix values
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' ca
    
    ## try to get the inverse
    m <- x$getinverse()
    
    ## if inverse isn't present
    if (is.null(m)) {
        message("cache miss - computing and storing ")
        
        ## get the inner matrix
        data <- x$get()
        ## calculate the inverse
        m <- solve(data, ...)
        ## store the result
        x$setinverse(m)
        return (m)
    } else {
        message("cache hit")
        return (m)
    }
}
