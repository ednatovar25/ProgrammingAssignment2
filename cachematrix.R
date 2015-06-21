
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The following two functions are a pair of 
## functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse.
#This function will set the value of the matrix, get the value of the matrix, 
#set the value of the inverse and get the value of the inverse.
#

makeCacheMatrix <- function(x = matrix()) {
  
    # initialize matrix as null
    cMatrix = NULL
  
    # create matrix in environment
    set <-function(y) {
      x <<- Y
      cMatrix <<- NULL
      
    }
    
    get<-function() x
    # store inverse of matrix in cache
    setinverse <- function(inverse) cMatrix <<- inverse
    getinverse <- function() cMatrix
    list (set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## get inverse of the matrix
      cMatrix = x$getInverse()
      
      #if cached matrix exists, return from cache otherwise
      #create it in the current environment
      if (!is.null(cMatrix)) {
        message("utilizing cached data")
        return(cMatrix)
      }
      
      #cached matrix not found, so create a matrix
      matrix <- x$get()
      cMatrix <- solve(matrix,...)
      x$setinverse(cMatrix)
      return(cMatrix)
}
