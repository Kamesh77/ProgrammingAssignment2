## This scipt is meant for caching the matrix inverse matrix calculation which is a system resource
## intensive calculation and subsequently use the cached solution wherever the same matrix 
## inversion solution is required. This approach reduces the overall time required for the execution
## of the script. This consists of two functions makecache matrix and cachesolve functions.

## makeCacheMatrix function - this function takes a matrix as its argument, setInverse component 
## calculates the inverse of the matrix, getInverse component caches the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
     
     invmatrix <- NULL
     
     set <- function(y) {
          x <<- y
          invmatrix <<- NULL
     }
     
     get <- function() x
     
     setInverse <- function(inverse) invmatrix <<- inverse
     
     getInverse <- function() invmatrix
     
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}


## cacheSolve function - this function first checks by calling the getInverse in makeCachematrix
## to check if there is already available a cached solution of the matrix inversion, if the
## cached solution is available it returns the available solution thereby saving execution time.
## If there is no cached solution it proceeds to calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        
     invmatrix <- x$getInverse()
     
     if (!is.null(invmatrix)) {
          message("getting cached data")
          return(invmatrix)
     }
     
     mat <- x$get()
     
     invmatrix <- solve(mat, ...)
     
     x$setInverse(invmatrix)
     
     return(invmatrix)
}

## TEST RESULTS
## first execution of cachesolve has calculated the matrix and stored in cached. Second execution
## has taken the solution from cache, we get the message getting cached data in execution.
##> test<-makeCacheMatrix(matrix(c(2,4,6,8),nrow=2,ncol=2))
##> cacheSolve(test)
##[,1]  [,2]
##[1,] -1.0  0.75
##[2,]  0.5 -0.25
##> cacheSolve(test)
##getting cached data
##[,1]  [,2]
##[1,] -1.0  0.75
##[2,]  0.5 -0.25
