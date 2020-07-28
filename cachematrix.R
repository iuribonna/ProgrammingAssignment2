## Coursera's R Programming Assignment 2
## Functions to compute by cache and show an inverse of a given matrix
## Based on code of RPub (https://rpubs.com/RprogameR/241424)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##the variable that will receive the matrix is initialized empty
 
##  Local function to assign variables to a different enviroment
   set <- function(y) 
   {
    x <<- y
    i <<- NULL
   }
   
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
## Creating a list of elements to set and get the values of the matrix and its
#  inversion
  
   list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes and return inversed matrix
## It takes the previous cached matrix (x) and additional info as arguments

cacheSolve <- function(x, ...) 
  {
## Retrieves the getInverse element from the given cached matrix
  i <- x$getinverse()

## data receives the value of the original matrix
  data <- x$get()
  i <- solve(data, ...)  ## solve() is the base R function to inverse a matrix
  x$setinverse(i)
  i ## the inversed matrix is printed
}
