## makeCacheMatrix takes data in the form of a matrix and creates a list object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {      # input x as a matrix
    m <- NULL                                    # inverse variable resets per function call
    set <- function(y) {                         # Function to clear cache
        x <<- y
        m <<- NULL
    }
    get <- function() {x}                        # assigns original value of matrix to 'get'
    setinverse <- function(solve) {m <<- solve}  # cacheSolve to assigns inverse into
                                                 # cache using superassignment
    getinverse <- function() {m}                 # accesed by cacheSolve to return
                                                 # cached inverse, or NULL
    list(set = set, get = get,                   # list with internal value defined functions
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve takes the list object created by makeCacheMatrix and computes its
## inverse. If, however, the matrix remains unchanged and the inverse has been
## chached then it returns the inverse from the cache.


cacheSolve <- function(x, ...) {                 # input object created by makeCacheMatrix
    m <- x$getinverse()                          # assign inverse value of object
    if(!is.null(m)) {                            # test if cached version exists
        message("getting cached data")           # if cached exists, send message
        return(m)                                # and return value, ending the function
    }
    data <- x$get()                              # else if 'm' returned as NULL
    m <- solve(data, ...)                        # compute inverse of matrix
    x$setinverse(m)                              # store the calculated value using
                                                 # superassignment within makeCacheMatrix
    m                                            # return value, ending the function
}