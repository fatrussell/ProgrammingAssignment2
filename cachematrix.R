## These two functions are used get the inverse of a matrix.  
## If previously determined, cached result will be returned.
## If not cache available, calculate it from matrix

## Create a special vector (list) containing functions to:
##
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(invrs) i <<- invrs
    getInverse <- function() i
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Calculate the inverse of the matrix, but first check cached data

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
