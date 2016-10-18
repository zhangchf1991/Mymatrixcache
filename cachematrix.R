####    Submission for PA2
##  This function will cache an inverse matrix for a given matrix if the inverse has ever been calculated 
##  If the inverse has never been set, then calculate a new one and fetch it

##  First try to set functions to set,get originial matrix and set, get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get  <- function() x
    setinv <- function(inv) inv_x <<- inv_x
    getinv <- function() inv_x
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}


## Try to find the cached inverse matrix, if none is find then calculate one

cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    matrix <- x$get()
    inv_x <- solve(matrix,...)
    x$setinv(inv_x)
    inv_x
}