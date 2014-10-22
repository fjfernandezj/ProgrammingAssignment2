## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix (containing a function)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<- function(solve) m <<- solve
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created above 
cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

b<- makeCacheMatrix(matrix(1:4,2,2))
b
b$get()
b$getinverse
b$getinverse()
cacheSolve(b)
b$getinverse()
b$set(matrix(3:6,2,2))
b$getinverse()
cacheSolve(b)
cacheSolve(b)
b$get()
cacheSolve(b)
