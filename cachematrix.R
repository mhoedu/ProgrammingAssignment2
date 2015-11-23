## makeCacheMatrix: take a input matrix x presumably inversible matrix then defines a special "matrix" object 
## that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
      x <<- y
      inverse_matrix <<- NULL
  }
  get <- function() x
  setinvmat <- function(inverse) inverse_matrix <<- inverse
  getinvmat <- function() inverse_matrix 
  list (set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)

}


## cacheSolve take an input "makeCacheMatrix" matrix x   
##    and determine whether or not the input matrix already
##    defined its inverse . If inverse is not defined then use solve to calculate
##    the inverse of x. If x's inverse matrix is already define, just grab it from 
##    its cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # test inverse matrix of x
  #is the inverse matrix already defined
  
   invX <- x$getinvmat()
   if (!is.null(invX)) {
    message("Inverse matrix is already defined. Getting it from cache data")
    return(invX)
   }
   else
   {
     message("Nothing in cache. Use solve to obtain the inverse of current matrix")
     invX <- solve(x$get())
     x$setinvmat(invX)
     invX
   }
}
####  Here is the test case(s) for this assignment.
#### define matrix  2x2 filling with index 1:4
#> testmatrix <- makeCacheMatrix(rbind(1:2,3:4))
#### Show it
# > testmatrix$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#### Initialize with cacheSolve, thus caching its inverse
#> cacheSolve(testmatrix)
#Nothing in cache. Use solve to obtain the inverse of current matrix
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#### Second call with cacheSolve we will get it from its inverse
#> cacheSolve(testmatrix)
#Inverse matrix is already defined. Getting it from cache data
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
