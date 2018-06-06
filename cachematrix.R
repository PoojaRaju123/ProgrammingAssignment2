## These two functions are all about finding the inverse of matrices and helping to avoid repetition on certain aspects like finding inverse 

##makeCacheMatrix will get a matrix to perform the function

makeCacheMatrix <- function(x = matrix()) 
  {
   inv <- NULL
   set <- function(y)
    {
    x <<- y
    inv <<- NULL
   }
   get <- function() x
   setInverse <- function(solveMatrix) inv <<- solveMatrix
   getInverse <- function() inv
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }


## cacheSolve is to find the inverse of the given matrix, if no such matrix is given then it will pass on the previous value to 
cacheSolve <- function(x, ...)
  {
   ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv))
      {
       message("getting cached data")
       return(inv)
      }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv 
      
  }
