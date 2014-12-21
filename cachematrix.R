## These functions compute the inverse of a matrix, using cacheing of the inverse to avoid repeated computation which is costly. 

# This function creates a special "matrix" object that can cache its inverse. The special matrix object is a list containing functions to do the following: 
#         1. set the value of the matix
#         2. get the value of the matrix
#         3. compute and set the inverse of the matrix
#         4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, 
             get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.  If the matrix has changed, then the inverse will have been set to NULL by the above function. 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
