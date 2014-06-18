## Script by joyjoanne. Date 6/18/2014
## Script contains a pair of functions that cache the inverse of a matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Function contains a list that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(nrow=1,ncol=1) 
       
    set <- function(y) {
        x <<- y
        m <<- matrix(NA,nrow=1,ncol=1) 
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.na(m[1,1])) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
