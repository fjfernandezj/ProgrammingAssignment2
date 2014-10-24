## These two functions compute the inverse of a matrix. 
## The inverse is cached on first computation, 
 

## This function creates and store a matrix 
## input: numeric matrix

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
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { 
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

##Testing functions
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
