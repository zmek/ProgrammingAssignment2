## Functions made for Week 3 of Introduction to R course

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # note m is a value that will be available in the parent env after the makeCacheMatrix completes; x and y are not
        set <- function(y) { #set is a function applied to the input  
                x <<- y
                m <<- NULL
        }
        get <- function() x #get is a function that gets the stored data from parent env
        setinverse <- function(inverse) m <<- inverse # store inverse of input and assigns it to m in the parent env (note - inverse could be any name)
        list(set = set, get = get,  #this creates a list of functions and returns them to the parent env
             setinverse = setinverse,
             getinverse = getinverse)
}

## Note - no calculations are performed within the makeCacheMatrix function. It is incomplete without the following function

## The CacheSolve function returns a matrix that is the inverse of 'x'
## Note - CacheSolve must have an input of type makeCacheMatrix (ie a list with same elements)

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()  #performs the getinverse() function on the input object 
                if(!is.null(m)) {   #if inverse has been stored already, m is !null, therefore retrive m from the input
                        message("getting cached data")
                        return(m)  #return and exit here
                }
                data <- x$get()  #otherwise get the data that has been stored in the input list
                m <- solve(data, ...) #calculate inverse
                x$setinverse(m) #save the inverse for next time
                m #return m
        
}
