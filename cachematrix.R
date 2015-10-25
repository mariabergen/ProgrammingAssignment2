
## The two functions together create a matrix first and then 
## calculate its inverse, saving the result in the cache. 
## Once the inverse has been calculated once,
## it is saved in the cache. So if we need the result again,
## it can be recovered from the cache instead of going through
## the calculations once more.

## makeCacheMatrix: Creates a matrix that can save 
## the value of its inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        ## the value of the matrix is set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x ## get the matrix
        setinverse <- function(solve) inv <<- solve ## define the inverse
        getinverse <- function() inv ## get the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}        
        

## cacheSolve: Returns the inverse of a matrix
## created by makeCacheMatrix
## The function checks the cache first
## If the inverse has not been calculated previously, it does it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() 
        ## check whether the inverse has already been calculated 
        if(!is.null(inv)) {
                message("getting the result from the cache")
                return(inv)
        } 
        ## if the inverse is not in the cache, 
        ## then go ahead and calculate it
        message("calculating the inverse")
        data <- x$get() ## get the matrix
        inv <- solve(data,...) ## calculate the inverse
        x$setinverse(inv) ## set the inverse for future use (in the cache)
        inv ## return the inverse
}
