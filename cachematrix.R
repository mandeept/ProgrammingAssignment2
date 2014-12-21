## These functions compute the inverse of a matrix, using cacheing of the inverse to avoid repeated computation which is costly. 

# This function creates a special "matrix" object that can cache its inverse. The special matrix object is a list containing functions to do the following: 
#         1. set the value of the matix
#         2. get the value of the matrix
#         3. compute and set the inverse of the matrix
#         4. get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. Ifthe inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
