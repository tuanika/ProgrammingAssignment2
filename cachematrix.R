## These two functions cache the inverse of a matrix:

## 'makeCacheMatrix' creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { # input x is a matrix
        
        i <- NULL #inverse of a matrix <- NULL everytime the function is called
        
        set <- function(y) { # takes an input matrix
                x <<- y # saves the input matrix back into x
                i <<- NULL # resets inverse matrix to NULL
        }
        get <- function() x  # returns the matrix 
        setinverse <- function(solve) i <<- solve 
        # sets inverse of a matrix, called by 'cacheSolve' at first access
        getinverse <- function() i 
        # returns inverse of a matrix at the second access of cacheSolve
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse) ## list of the internal functions
}

## 'cacheSolve' computes the inverse of the special "matrix" returned by
## 'makeCacheMatrix' or retrieves inverse from chache if it has been calculated
cacheSolve <- function(x, ...) { 
        ## the input x is an object created by 'makeCacheMatrix' function
        
        i <- x$getinverse() # accesses the object x and gets its inverse
        
        if(!is.null(i)) {  
                # if inverse was already cached (not NULL):
                message("getting cached data")
                return(i)
        }
        
        # if inverse is NULL
        data <- x$get() # get the initial matrix 'x'
        i <- solve(data, ...) # calculate the inverse
        x$setinverse(i) # store the calculated inverse (see 'makeCacheMatrix')
        return(i)
}
