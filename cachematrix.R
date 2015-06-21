## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
set.seed(1)
r = rnorm(9)
mat1 = matrix(r, nrow=3, ncol=3)

tmp_list <- makeCacheMatrix(mat1)
cacheSolve(tmp_list)
cacheSolve(tmp_list)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i## Return a matrix that is the inverse of 'x'
}

