##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

#The makeCacheMatrix creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
                
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        
        
        
        ## The cacheSolve below computes the inverse of the special "matrix" created above
        
        
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inv <- x$getInverse()
                if(!is.null(inv)){
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data)
                x$setInverse(inv)
                inv      
        }
        