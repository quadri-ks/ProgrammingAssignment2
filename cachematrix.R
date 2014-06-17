## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initialize the matrix
        invx <- NULL
        
        # set the values for the matrix
        set <- function(y){
                
                # sets the matrix in parent environment 
                x <<- y
                # sets the inverse to null whenever new matrix gets set
                invx <<- NULL
        }
        
        # returns the matrix present in current environment
        get <- function() x
        
        # sets the inverse of the matrix in parent environment
        setinverse <- function(inverseMatrix) invx <<- inverseMatrix
        
        # retruns the inverse of matrix x i.e invx present in the parent environment
        getinverse <- function() invx
        
        # returns the special matrix object with functions
        list(set= set , get = get , 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        
        if (!is.null(inv) ) {
                message ("returning cached inverse")
                return(inv)
        }
        
        # if inverse of matrix x is not calculated or new value of matrix is set 
        # below code will get executed
        
        data <- x$get()
        
        #computes the inverse of matrix 
        inv <- solve(data)
        
        # sets the inverse of matrix in cache
        x$setinverse(inv)
        
        #returns the inverse of matrix
        inv

}
