## makeCacheMatrix: Defines a matrix and sets its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## the initial value is null
        ## the value of the matrix is set
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        ## 
        get <- function() x ## get the matrix
        setinverse <- function(solve) inv <<- solve ## define the inverse
        getinverse <- function() inv ##get the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}        
        

## cacheSolve: Returns a matrix that is the inverse of 'x'
## Check the cache first
## If the inverse has not been calculated previously, do it

cacheSolve <- function(x, ...) {
  
        inv <- x$getinverse() ## define the inverse to look for
        ## check whether the inverse has already been calculated 
        ## using an "if" loop
        if(!is.null(inv)) {
                message("getting the result from the cache")
                return(inv)
        } 
        ## if the inverse is not in the cache, 
        ## then go ahead and calculate it
        message("calculating the inverse")
        data <- x$get() ## get the matrix
        inv <- solve(data,...) ## define the inverse
        x$setinverse(inv) 
        inv ## return the inverse
}
