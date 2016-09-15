##The first function, makeVector creates a special "vector", which is really a list containing a function to
##Set the value of the matrix
##Get the value of the matrix
##Set the value of the inverse of the matrix (solve())
##Get the value of the inverse of the matrix (solve())




makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL 
     set <- function(y) { 
         x <<- y 
         inv <<- NULL 
     } 
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse 
     getinverse <- function() inv 
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
 } 


##cacheSolve() calculates the inverse of the matrix created with makeCacheMatrix(). 
##It checks if the inverse has already been calculated. 
##If so, it retrieves the inverse of the matrix cache rather than performing the calculation. 
##Otherwise, it calculates the inverse of the matrix x and saves inverse of the matrix x
##in the cache via the setinverse function.

 cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
     data <- x$get() 
     inv <- solve(data) 
     x$setinverse(inv) 
    inv 
 } 