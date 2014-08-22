## Compute inverse of matrix, store result in global cache.
## When called to compute inverse, returned cached version if exists, else compute.


## from input matrix x, create special global matrix m as cache to store inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv<- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## check whether inverse exists in m.  If so retieve from cache, if not calculate inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
