## The following two functions are used to create an object that
## stores a matrix, computes its inverse, and caches it.

## The first function, `makeCacheMatrix` creates a list containing
## a function that will:
##     1.  set the value of the matrix
##     2.  get the value of the matrix
##     3.  set the value of the inverse
##     4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinvM <- function(inverse) inv <<- inverse
            getinvM <- function() inv
            list(set = set, get = get,
                 setinvM = setinvM,
                 getinvM = getinvM)
}


## The second function calculates the inverse of a matrix processed with
## the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it get`s the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the mean in the cache via the `setinvM` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinvM()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinvM(inv)
            inv
}
