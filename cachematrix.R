## These two functions allow to store the inverse of a matrix, to avoid 
## repeating its calculation


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## For a makeCacheMatrix object, looks if the inverse of the matrix
## has been already calculated (and returns it). If the inverse of 
## the matrix does not exist, it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
