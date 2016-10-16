## Put comments here that give an overall description of what your
## functions do

## function that caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv_x <- NULL

   # setters
   set <- function(matrix) {
      x <<- matrix
      inv_x <<- NULL
   }

   # set inverse matrix
   setinv <- function(inverse) inv_x <<- inverse

   # getters
   get <- function() x
   getinv <- function() inv_x

   list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## takes a matrix, calculates the inverse and caches it

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv_x <- x$getinv()

   if (is.null(inv_x)) {
      message("calculating inverse and caching")
      data <- x$get()
      inv_x <- solve(data, ...)
      x$setinv(inv_x)
   }

   return(inv_x)
}
