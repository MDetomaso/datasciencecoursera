## This function creates a special "matrix" object that can cache its inverse
## The following two functions are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a "matrix" object and caches its inverse


makeCacheMatrix <- function(x = matrix()){
    i <-NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the "matrix" created by 
## makeCacheMatrix's function above. If the inverse has already been calculated 
##(for the same matrix), then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
    i <-x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matr <- x$get()
    i <- solve(matr, ...)
    x$setinverse(i)
    i
}

