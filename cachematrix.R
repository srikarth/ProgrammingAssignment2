## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    set = function(m)
    {
      x <<- m
      inv <<- NULL
    }
    
    get = function() {x}
    setInverse = function(inverse) {inv <<- inverse}
    getInverse = function() {inv}
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

 ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 ## If the inverse has already been calculated (and the matrix has not changed),
 ## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {      
      ##Return a matrix that is the inverse of 'x'
   inv = x$getInverse()
   if(!is.null(inv))
   {
     message("getting cached data")
     return(inv)
   }


   m = x$get()   
   inv = solve(m)
   x$setInverse(inv)
   inv 
}

sqMtx = matrix(sample.int(15, 10*10, TRUE), 10, 10)


c = makeCacheMatrix(sqMtx)


cacheSolve(c)
