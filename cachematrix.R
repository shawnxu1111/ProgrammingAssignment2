## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() returns a list of four subfunctions
## i.e., $set(y), $get(), $setsolve(solve) and $getsolve() 
## the matix x can be set at the first call of the function or by $set(y) 
## and its solve m is gonna to be set as NULL at the same time.
## x can be achieved from cache by $get().
## the solve m of x can be stored into cache by $setSolve(solve).
## the solve m of x can be achieved back from cache by $setSolve(solve) and 
## if m has never been set, a NULL will be return.

makeCacheMatrix <- function(x = matrix()) {  
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        
        get <- function() x 
        
        setsolve <- function(solve) m <<- solve
        
        
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

## The following function calculates the inverse of the matrix 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the solve in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve() ##
        
        if(!is.null(m)) {  
                ##if the inversse of matrix x has exists skip the calculation,
                ##print a message and return m
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
