## Assignment: Matrix Inversion.
## Matrix inversion tends to use a large amount of computational power of the computer
## As a result, it is beneficial to save this off in memory is the computation is already 
## done. This routine will store in memory the inverse of the value being passed.
## If the inverse value has been calculate the cache solve function will be called.



## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The Cachesolve function : If the matrix has already been calculated and has not changed  it will 
## Cachesolve will return the value from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

x = rbind(c(1, -1/4), c(-1/4, 1)) 
m = makeCacheMatrix(x) 
m$get() 

cacheSolve(m)

x = c(1, -5)
m = makeCacheMatrix(x) 
m$get() 
cacheSolve(m)
