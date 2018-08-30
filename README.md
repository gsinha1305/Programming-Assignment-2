# Programming-Assignment-2
## R Programming Assignment 2 on Lexical Scoping




## Creating the cachematrix and the inverse of the function for the matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){												                        ##set the value
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(solveMatrix) inv <<- solveMatrix						##set the value of matrix inversion
getInverse <- function() inv											                ##get the value of matrix inversion
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)}





## Creating the cachesolve to inverse the matrix
cacheSolve <- function(x, ...) {
inv <- x$getInverse()
if (!is.null(inv)) {                                               ##check if the value has already been calculated
message("getting cached data")                                     ##already calculated and skip the process
return(inv)
}
mat <- x$get()                                                     ##if not already calculated
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}






## INPUT:
mtrx <- makeCacheMatrix(matrix(1:4,2,2))
mtrx$get()
mtrx$getInverse()
cacheSolve(mtrx)
