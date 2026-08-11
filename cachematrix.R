## The makeCacheMatrix function holds a special matrix and its cached inverse matrix. 
## The cacheSolve function checks the cache within makeCacheMatrix, calculates the inverse matrix if it is missing, and stores the result within makeCacheMatrix.



## makeCacheMatrix creates a special matrix (which is actually a list containing 4 functions - one to set the value of the matrix, one to get the value of the matrix, one to set the inverse of the matrix, and one to get the inverse of the matrix).
## makeCacheMatrix must be run in the format randomname <- makeCacheMatrix(matrix(c#1,#2,#3,#4), nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) { ##set up function to take matrix x (defaults to empty)
inv <- NULL ##set up a variable in to hold the cached inverse matrix, initially empty
set <- function(y) { ##Allows you to change the stored matrix
  x <<- y ##Updates the stored matrix
  inv <<- NULL ##resets in to NULL, as new matrix means the old cached inverse is no longer valid
}
get <- function() x ##returns whatever x currently is
setinverse <- function(inverse) inv <<- inverse ##stores inverse value into inv
getinverse <- function() inv ##returns whatever is cached in in
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##storage of 4 functions in list format to allow later usage of $ in cacheSolve
}



## cacheSolve calculates the inverse of the matrix created using makeCacheMatrix. If there is already an inverse matrix cached in makeCacheMatrix, it will retrieve that; otherwise it will calculate it.
## cacheSolve can be run using cacheSolve(randomname)

cacheSolve <- function(x, ...) { ##x is parameter for the list returned by makeCacheMatrix; ... allows passage of other arguments
inv <- x$getinverse() ##retrieve the inverse value from makeCacheMatrix and store it in local variable  
if(!is.null(inv)) { ##runs if value stored within inv is NOT null
  message("getting cached data") 
  return(inv) ## returns cached value from inv, immediately exits function
}  
data <- x$get() ##retrieves matrix from makeCacheMatrix
inv <- solve(data, ...) ##computes the inverse matrix; overwrites local inv with this value
x$setinverse(inv) ##pushes new mean into makeCacheMatrix's inv variable
inv ##returns a matrix that is the inverse of 'x'
}
