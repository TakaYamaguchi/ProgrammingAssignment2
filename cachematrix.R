## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

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

#The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

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

#Eample to run this functions
set.seed(111)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)

tmp_list <- makeCacheMatrix(mat1)
cacheSolve(tmp_list) #first run
cacheSolve(tmp_list) #second run and takes the cache


