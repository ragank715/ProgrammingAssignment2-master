## The flow of function is very similar to what was provided
## for the example with the mean

## Chnages implemented are as follows:
## 1) a variable n is introduced defining the dimension of square matrix
## 2) Instead of NULL, NA is being used to fill in the inverted matrix for
## the time inverse is not computed
## 3) mean fucntion has been replaced by solve
## 4) variable names have been chaged to represent the inversion of materix

makeCacheMatrix <- function(x = matrix(,n,n)) {
	xinv <- matrix(NA,n,n)
        set <- function(y) {
                x <<- y
                xinv <<- matrix(NA,n,n)
        }
        get <- function() x
        setinverse <- function(solve) xinv <<- solve
        getinverse <- function() xinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x, n, ...) {
	xinv <- x$getinverse()
	        if(!is.na(xinv)) {
	                message("getting cached data")
	                return(xinv)
	        }
	        data <- x$get()
	        xinv <- solve(data, ...)
	        x$setinverse(xinv)
        xinv
}

