## This function accepts an invertable matrix as its argument.
## It defines an object that contains the matrix and functions
## to calculate and store the inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   ## define a function that stores the matrix
   set <- function(y) {
      x <<- y
      ## ensure inverse is set to NULL when setting a new matrix
      inv <<- NULL
   }
   ## define a function to return the defined matrix
   get <- function() x
   ## define a function to assign the calculated inverse to an object
   setinverse <- function(inverse) inv <<- inverse
   ## define a function to return the precalculated inverse
   getinverse <- function() inv
   ## return a list of the functions defined above
   list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function accepts an object defined by makeCacheMatrix.
## If an inverse of the matrix has been calculated, it is returned.
## If not, the inverse is calculated, stored in the object, and returned. 

cacheSolve <- function(x, ...) {
   ## Call the getinverse function of object x and assign the result to inv
   inv <- x$getinverse()
   ## If inv is not NULL return it and exit the function
   if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   ## If no inverse has yet been stored, use the get function of object x
   ## to retrieve the matrix
   data <- x$get()
   ## Calculate the inverse and store it in inv
   inv <- solve(data, ...)
   ## Use the setinverse function of object x to store the inverse in object x
   x$setinverse(inv)
   ## Return a matrix that is the inverse of 'x'
   inv
}
