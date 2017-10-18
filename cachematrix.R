## Functions made for Week 3 of Introduction to R course

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse)
}

## Note - no calculations are performed within the makeCacheMatrix function. It is incomplete without the following function

## The CacheSolve function returns a matrix that is the inverse of 'x'
## Note - CacheSolve must have an input of type makeCacheMatrix (ie a list with same elements)

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse() 
                if(!is.null(m)) {   
                        message("getting cached data")
                        return(m)  
                }
                data <- x$get()  
                m <- solve(data, ...) 
                x$setinverse(m) 
                m 
        
}
