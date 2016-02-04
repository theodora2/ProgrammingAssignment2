## the first function creates a special object (matrix) and the second function 
# calculates the inverse of the special "matrix".which is a list containing a function to
# 1. set the value of a matrix 2. get the value of a matrix 
# 3. set the value of inverse 4.get the value of inverse

## this function is a list containing a function to
# 1. set the value of a matrix 2. get the value of a matrix 
# 3. set the value of inverse 4.get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
   
     i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## this function first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse matrix from the cache and skips the computation.  
# Otherwise, it calculates the inverse matrix of the data and sets the value 
# of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}

# ex. to call the functions
# dd<-matrix(1:4, 2, 2)
# ff<-makeCacheMatrix(dd) 
# cachesolve(ff)
