## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the output variable independent of getting initialized 
  ## through a separate set function call...
  inv <- NULL
  ## component#1:
  ## a function that sets the initial values for further processing...
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## component#2:
  ## a function that gets the matrix data when requested...
  get <- function() x
  ## component#3:
  ## a function to set the computed inverse of a matrix...
  setInverse <- function(inverse) inv <<- inverse
  ## component#4:
  ## a function to get the computed inverse of a matrix...
  getInverse <- function() inv
  ## a vector that lists the components of this function when
  ## an object of this function is queried...
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The following function computes the inverse of the matrix
## created using the above function. However, it first
## checks to see if the inverse has already been computed.  If so,
## it directly gets the inverse from the cache using the getInverse
## function and skips the computation. Otherwise, it computes the 
## inverse of the data and sets the value of the inverse in the cache
## using the setInverse function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'(the given matrix)
  inv <- x$getInverse()
  ## checks to see if an inverse matrix exists...
  if (!is.null(inv)) {
    ## if exists; returns the inverse matrix...
    message("getting cached data")
    return (inv)
  }
  ## otherwise; gets the given matrix...
  data <- x$get()
  ## computes the inverse of the matrix...
  inv <- solve(data, ...)
  ## makes a copy of the computed inverse of the matrix for future get requests on the object...
  x$setInverse(inv)
  ## returns the inversed matrix...
  inv
}

## Tests:
## > source('~/ProgrammingAssignment2/cachematrix.R')
## Create a "3X3" matrix
## > test_mat <- matrix(c(1,12,23,-4,15,-62,72,8,19),3,3)
## Printing the matrix's contents...
## > test_mat
##     [,1] [,2] [,3]
##[1,]    1   -4   72
##[2,]   12   15    8
##[3,]   23  -62   19
##> cache_mat <- makeCacheMatrix(test_mat)
## Printing the object's contents. A call to the object should list
## the components...
##> cache_mat
##$set
##function (y) 
##{
##    x <<- y
##    inv <<- NULL
##}
##<environment: 0x00000000051b7c40>
##
##$get
##function () 
##x
##<environment: 0x00000000051b7c40>

##$setInverse
##function (inverse) 
##inv <<- inverse
##<environment: 0x00000000051b7c40>

##$getInverse
##function () 
##inv
##<environment: 0x00000000051b7c40>
## Call the component get on the object. It should get the matrix 'test_mat'
##> cache_mat$get()
##     [,1] [,2] [,3]
##[1,]    1   -4   72
##[2,]   12   15    8
##[3,]   23  -62   19
## Next, call the function 'cacheSolve' compute an inverse.
##> cache_inv <- cacheSolve(cache_mat)
## Print the computed inverse matrix's contents...
##> cache_inv
##              [,1]         [,2]          [,3]
##[1,] -0.0100837949 0.0566551755  0.0143574647
##[2,]  0.0005681011 0.0211359440 -0.0110521491
##[3,]  0.0140605028 0.0003873417 -0.0008134175
## Try to retrieve the same using the 'getInverse' component call on the catche_mat object...
##> cache_mat$getInverse()
##              [,1]         [,2]          [,3]
##[1,] -0.0100837949 0.0566551755  0.0143574647
##[2,]  0.0005681011 0.0211359440 -0.0110521491
##[3,]  0.0140605028 0.0003873417 -0.0008134175
## a few additional tests...
## setting the inversed matrix object 'cache_mat' to NULL
## Calling the 'getInverse' on the object to check whether it gets anything...
## Got an ERROR...! As expected...:)
##> cache_mat$getInverse()
##Error: attempt to apply non-function
##> cache_mat$get()
##Error: attempt to apply non-function
## Once again, calling the following WORKED...
##> cache_mat <- makeCacheMatrix(test_mat)
##> cache_inv <- cacheSolve(cache_mat)
##> cache_mat$get()
##> cache_mat$getInverse()


