##The objective is to  write two functions that allow us to cache the inverse of a matrix

##Arguments: 
##x: a matrix (optional)
##Returns: a matrix containing functions to
#1. set/get the value of the matrix
#2. set/get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This function allows us to compute the inverse of a matrix. If it´s been
#already computed, the cached inverse matrix is returned

##Arguments:
#x: a matrix
#...: additional arguments

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting inverse matrix")
        return(i)
    }
    lol <- x$get()
    i<- solve(lol, ...)
    x$setinverse(i)
    i
    }
