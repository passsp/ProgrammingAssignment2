#the function makeCacheMatrix takes a matrix x
#and returns a list of four elements which are described below

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      #the function 'set' sets a first element of a list to be returned 
      #to its argument, and inverse of the argument to NULL  
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      #the function 'get' returns original matrix from which
      #our object was constructed
      get <- function() x
      #the function 'setInverse' sets the third element which corresponds
      #to the inverse of the original matrix to its argument
      setInverse <- function(Inverse) I <<- Inverse
      #'getInverse' returns current value of inverse of a matrix
      getInverse <- function() I
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


#The next function calculates inverse of a matrix A.
#In order to apply it, one has to construct special object
#by calling makeCacheMatrix(A). cachInverse takes an object
#returned X by 'makeCacheMatrix(...)' and returns inverse of
#the matrix X$get()
cacheInverse <- function(x, ...) {
      I <- x$getInverse()
      #if inverse for x has been already calculated,
      #it is simply returned as a result
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      #otherwise, inverse is calculated
      data <- x$get()
      I <- solve(data)
      x$setInverse(I)
      I
}

#an example of use of our functions
A <-matrix(c(3,2,2,4), 2, 2)
x <- makeCacheMatrix(A)
#the first call of 'cacheInverse' will 
#calculate the inverse of A
cacheInverse(x)
#calling cacheInverse for the second
#time, we see the message "getting cached data"
cacheInverse(x)
