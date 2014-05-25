## The following two functions assist with the potentially time consuming
## computation of finding the inverse of a matrix with the solve function.
## Instead of recalculating the inverse for the same matrix submitted more than once
## the original result is recalled from cache rather than recalculated.

## This first function creates a special matrix which is a list containg a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         get <- function() x
         setinverse <- function(solve) m <<- solve
         getinverse <- function() m
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## The second function calculates the inverse of the special matrix above but
## first checks to see if it has already been calculated and if so gets the inverse 
## from the cache and skips the computation. Otherwise it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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
