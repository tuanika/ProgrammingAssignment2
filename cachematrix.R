## These two functions calculate and cache the inverse of a matrix:

## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { # input x is a matrix
        
        i <- NULL
        set <- function(y) { 
                x <<- y 
                i <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve 
        getinverse <- function() i 
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## 'cacheSolve' returns inverse calculated from the special "matrix" returned by 
## 'makeCacheMatrix' or retrieves inverse from chache if it has been calculated
cacheSolve <- function(x, ...) { 
        
        i <- x$getinverse() # accesses the object x and gets its inverse
        if(!is.null(i)) {  
                message("getting cached data")
                return(i) 
        }
        # calculate the inverse and store it back in x
        data <- x$get() 
        i <- solve(data, ...) 
        x$setinverse(i)
        return(i)
}
