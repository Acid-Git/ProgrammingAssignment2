#makeCacheMatrix creates a special matrix that can store its inverse matrix inside the object

# usage:
# x <- makeCacheMatrix()to create a new object (hint: inverseMatrix will be initialised to NULL)
# x$set(matrix(1:4, 2)) will initialize the matrix to a 2x2 matrix (inverse still set to NULL)
# x$get() will return the saved original matrix
# x$getInverseMatrix() will return the saved inverse matrix
# x$setInverseMatrix("solved Matrix) will set the inverse matrix to the value

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL   #init the inverse matrix to be NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x  #get stored value of original matrix
  
  setInverseMatrix <- function(solve) inverseMatrix <<- solve #store the inverse matrix
  getInverseMatrix <- function() inverseMatrix                #get the inverse matrix
  
  list(set = set,
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


#cacheSolve will calulate and store the inverse matrix defined by argument
#If inverse matrix already stored this value is returned, otherwise the function calculate and store it

cacheSolve <- function(x, ...) {
  
  tmp <- x$getInverseMatrix()  #get current value of inverse matrix from argument
  
  #if matrix is already set to != NULL return saved value
  if(!is.null(tmp)) {
    message("getting cached data")
    return(tmp)
  }
  
  #otherwise get original matrix, solve it and store the inverse matrix
  originalMatrix <- x$get()
  tmp <- solve(originalMatrix)
  tmp <- x$setInverseMatrix(tmp)
  
  #return inverse matrix
  tmp
}
