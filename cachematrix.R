## The following two functions create (1) a list of functions that can cache
## the inverse of a matrix and (2) calculate that inverse either directly or
## calling the cached value if available

## This function creates a list, of which each element is one of the functions
## set, get, setinv, and getinv, which assign or call inverse values to objects

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function checks if an inverse is already available using getinv
## and returns that if available. If not, it calls the matrix itself and
## calculates the inverse directly using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
