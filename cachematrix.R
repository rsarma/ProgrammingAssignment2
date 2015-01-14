## Optimizing the fuction to inverse a matrix by using caching strategies
## Scoping is used to optimize


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# usage makeCacheMatric(inputMatrix)
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix
# usage cacheSolve(inputMakeCacheMatrix)
# The first call will return matrix inverse by calculating the invers
# The first call will also return the inverse but will -
# - return from the cache cache matrix but return from cache  using x$getinverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

##Output from test run
#> x <- matrix(seq(1:4), 2)
#> mtx <- makeCacheMatrix(x)
#> mtx$get()
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#First Run - not from cache
#> cacheSolve(mtx)
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#Second run from cache
#> cacheSolve(mtx)
#getting cached data.
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
