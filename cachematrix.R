## makeCacheMatrix contains four functions
## set, get, setinverse, and getinverse
## set substitutes the matrix x with y in the parent function & restores m to NULL
## get is a function that just returns the matrix x stored in the main function
## setinverse & getinverse simply store the value of the input in m and return it
## all four functions are stored in the main function using a list, the main
## function can then be treated as an object and functions called using $ opertor

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) { 
                x <<- y 
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve verifies that the value m, if it exists it returns the supposed
## inverse of the input matrix
## if it does not exist, it gets the input matrix and assigns it to data
## then the inverse is calculated using x$setinverse

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
        ## Return a matrix that is the inverse of 'x'
}
