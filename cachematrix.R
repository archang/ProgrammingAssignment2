## Function makeCacheMatrix takes in an input matrix x and returns a list of 
##      getter/setter functions of matrix x. It also includes getter/setter
##      functions of the inverse of x
## Function cacheSolve in an input y (the output of function makeCacheMatrix)
##      and if the inverse is saved to the object of y, the function returns it.
##      If that inverse is not found, then cacheSolve computes the inverse, 
##      and saves it to y

## makeCacheMatrix returns a "special matrix object" which is a list of
##      functions corresponding to the getting/setting of the supplied matrix
##      along with its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        getInverse <- function() m
        setInverse <- function(invToSet) m <<- invToSet

        list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve takes in the "special matrix object" returned by makeCacheMatrix.
##      If this "special matrix object" already has an inverse computed,
##      then cacheSolve returns the inverse. Otherwise, it computes the inverse
##      (the original matrix assumed to be invertible), saves it to the 
##      "special matrix object" and returns it to the screen

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                print("getting from cache...")
                return(m)
        }
        data <- x$get()
        invData <- inv(data)
        x$setInverse(invData)
        invData
}