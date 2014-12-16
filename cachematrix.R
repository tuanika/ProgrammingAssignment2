## These two functions calculate and cache the inverse of a matrix:

## 'makeCacheMatrix' creates a special "matrix" object that can cache 
## inverse of the matrix
makeCacheMatrix <- function(x = matrix()) { # input x is a matrix
        
        i <- NULL #inverse is set to NULL everytime 'makeCacheMatrix' is called
        
        set <- function(y) { # input y is a matrix
                x <<- y 
                i <<- NULL # inverse is reset to NULL if x changes
        }
        get <- function() {x} # returns the matrix
        
        setinverse <- function(solve) {i <<- solve}  
        # sets inverse of a matrix, called by 'cacheSolve' at first access
        
        getinverse <- function() {i} 
        # returns inverse of a matrix at the second access of cacheSolve
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse) # list of the internal functions
}

## 'cacheSolve' returns inverse calculated from the special "matrix" returned by 
## 'makeCacheMatrix' or retrieves inverse from chache if it has been calculated
cacheSolve <- function(x, ...) { 
        
        i <- x$getinverse() # accesses the object x and gets its inverse i
        
        if(!is.null(i)) {  
                
                #if inverse exists it is returned
                message("getting cached data")
                return(i) 
        }
        
        # if inverse is NULL then it is calculated, stored back in x and returned
        data <- x$get() 
        i <- solve(data, ...) 
        x$setinverse(i)
        return(i)
}
