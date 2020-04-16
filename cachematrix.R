## This R function caches the Inverse of a matrix in memory to avoid recalculation 
## when the inverse is requested for the same matrix again 
## or calculate a new inverse if the matrix changes and caches the result in memory. 

## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
