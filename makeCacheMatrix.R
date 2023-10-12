makeCacheMatrix <- function(x = matrix()) {
##create a matrix    
    i <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
##create inverse matrix
    getinverse <- function() i
    setinverse <- function(inverse) {
        i <<- inverse
    }
    return(list(set = set, get = get, 
                getinverse = getinverse, setinverse = setinverse))
}

cacheSolve <- function(x, ...) {
    ## Сhecking the cache for the presence of an inverse matrix
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    mat <- solve(x$get())
    x$setinverse(mat)
    return(mat)
}