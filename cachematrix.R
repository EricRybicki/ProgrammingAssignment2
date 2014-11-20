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



}