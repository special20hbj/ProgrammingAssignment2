## Coursera R / Week 3 Programming Assignment 2
## Caching the Inverse of a Matrix
## Here is a pair of functions that create a special "matrix" object that cache the inverse of the matrix
## in order to avoid repeatedly computing

## makeCachMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        ## 'x' is the variable of matrix defined within this function
        inv <- NULL
        ## 'inv' is the value of inverse in this function
        ## 1. Set the value of the matrix (y) 
        ## The <<- operator can be used to assign a value to an globle environment object 
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## 2. Get the value of the matrix (x)
        ## This is a constant function
        get <- function() x
        ## 3. Set the value of the inverse of the matrix (inv)
        setInverse <- function(inverse) inv <<- inverse
        ## 4. Get the value of the inverse of the matrix (inv) 
        ## The "makeCachMatrix" function returns a list that contains four elements
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## set 'inv' 
        ## Calling the function of 'getInverse()' to look up the inverse from the cache
        ## if it has already been calculated (and the matrix has not changed)
        inv <- x$getInverse()
        if(!is.null(inv)) {
                ## if 'inv' doesn't return NULL, then the inverse has already been calculated
                ## Return a matrix that is the inverse of 'x' (inv) 
                message("getting cached data")
                return(inv)
        }
        ## If the inverse has not been calculated from the cache
        ## Use the function of 'get()' to calling the data of variable 'x'
        data <- x$get()
        ## Get the value of the inverse of the matrix
        inv <- solve(data, ...)
        ## Use the function of 'setInverse()' to cache the value above in the variable 'x'
        ## Return a matrix that is the inverse of 'x' (inv)
        x$setInverse(inv)
        inv
}