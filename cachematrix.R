#-----------------------------------------------------------------------------#
# R Programming coursera course 
# author     : Emilia 
#last edited : 19-07-2014
#-----------------------------------------------------------------------------#

## makeCacheMatrix --> create special object to catch matrix inverse
## cacheSolve      --> extract inverse matrix or calculate if needed

## create class for catching the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix<- NULL # inverse matrix (default: NULL)
  
  #inverse matrix destructor and matrix constructor
  set <- function(y) {
    x <<- y
    inv_matrix<<- NULL
  }
  
  # get matrix 
  get <- function() x
  
  # set inverse matrix
  setinv <- function(solve) inv_matrix<<- solve
  
  # get inverse matrix 
  getinv <- function() inv_matrix
  
  #create list of functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## if inv_matrix is null, calculate inverse matrix, otherwise catch the inverse
cacheSolve <- function(x, ...) {
  
  # get inv_matrix
  inv_matrix<- x$getinv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  # calculate inv_matrix
  if(is.null(inv_matrix)){
  data <- x$get()
  inv_matrix<- solve(data)
  x$setinv(inv_matrix)
  return(inv_matrix)
  }
}

#-----------------------------------------------------------------------------#
# testing program                                                             #
#-----------------------------------------------------------------------------#

## calculate the inverse matrix (testing)
data<-matrix(c(1,2,3,4), nrow = 2, ncol = 2)  # create matrix 
matrix_object<-makeCacheMatrix(data)          # create special object

matrix <- matrix_object$get()                 # get matrix
matrix_inv <- matrix_object$getinv()          # try to get inverse matrix
cacheSolve(matrix_object)                     # calculate inverse matrix                                             
matrix_inv <- matrix_object$getinv()          # get inverse matrix

data1<-matrix(c(5,6,7,8), nrow = 2, ncol = 2)
matrix_object$set(data1)                      # set new matrix
matrix1 <- matrix_object$get()                # get the matrix 
cacheSolve(matrix_object)                     # calculate inv matrix
matrix_inv1 <- matrix_object$getinv()         # get inverse matrix
