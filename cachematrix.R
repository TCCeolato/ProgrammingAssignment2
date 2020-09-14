## The program works like the example that calculate the mean of a vector 
## and uses cache data to save time in repeated tasks


## This functions creates an object that cans cache the value of 
## his inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y    
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function returns a matrix that is the inverse of 'x', 'x' need to be 
## a result of makeCacheMatrix() function, if the inverse was already calculated
## by cacheSolve(), the function should return the value in 'x$getinv' and
## print the message "getting cached data", otherwise cacheSolve() calculates
## the inverse of 'x' (with solve(x)) and cache the value in 'x$setinv'
 
cacheSolve <- function(x, ...) {
    m <- x$getinv()  
    if(!is.null(m)) {   #if m is not null, send the message and returns m
        message("getting cached data")
        return(m)
    }
    data <- x$get()  #if m is null, takes the value of object x (in get)
    m <- solve(data, ...) # calculates the inverse of x and assigns to m
    x$setinv(m)  # set 'setminv' to new m
    m  #returns m
}
## Test data:
## data2x2 <- c(2,5,1,3)
## data3x3 <- c(1,0,0,2,1,0,3,4,1)
## matrixSqr <- matrix(data3x3, nrow = 3, ncol = 3)
## matrixMade <- makeCacheMatrix(matrixSqr)
## cacheSolve(matrixMade)

