## These functions provide the ability to cache the inverse of a matrix 
## so it can be retrieved from the cache rather than re-calculating each time

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<-y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function () i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse=getinverse)

}


## This function returns the inverse of the matrix 'x'. If the inverse is
## already cached, it retrieves the inverse from the cache. If not, the function
## computes the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
       if(!is.null(i)){
               message("getting cached data")
               return(i)
       }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
