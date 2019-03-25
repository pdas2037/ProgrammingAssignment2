## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Below pair of functions will cache the inverse of a matrix.

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                ## assigning the input argument to the x object in the parent environment
                x <<- y
                ## assigning NULL to inv in the parent environment. it clears any 
                ##value of inv that had been cached by a prior execution of cacheSolve().
                inv <<- NULL
        }
        ## getter for x
        get <- function() x
        
        ## sets the input argument to matrix inverse object in the parent environment
        setinv <- function(inv_val) inv <<- inv_val
        
        ## getter for the inverse
        getinv <- function() inv
        
        ## return object is a list which contains the above defined functions as 
        ## the elements
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. It takes a `makeCacheMatrix` object as input 
## and calculates the inverse If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## if inverse is not NULL, then gets from the cache without computation
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## else calculates inverse 
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
