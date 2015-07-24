## Following two functions will calculate inverse of a matrix and cache it.
## If matrix hasn't changed then inverse of the matrix will be fetched from
## the cache otherwise inverse will be calculated and cached.

## makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Parameter : x - is an invertible matrix.
    ## Output : This function returns a list of functions
    ##            1. set to set the matrix
    ##            2. get to get the matrix
    ##            3. setinv to set the inverse
    ##            4. getinv to get the inverse
  
    ## set inverse to NULL
    i <- NULL
    
    ## Define return functions
    set <- function (y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    
    ## Return functions.
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Parameter : x - output of makeCacheMatrix
    ## Output : Inverse of the original matrix.
  
    i <- x$getinv()
    
    ## If inverse exists in cache then return it otherwise calculate inverse,
    ## cache it and then return.
    if (!is.null(i)) {
        message("Getting inverse from Cache")
        return(i)
    }
    
    data <- x$get()
    i = solve(data, ...)
    
    x$setinv(i)
    return(i)
}
