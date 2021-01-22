##This function, `makeCacheMatrix` creates a special "matrix", which can
##cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##arguments within function
  inv=NULL                                  ##initialize inv as NULL; will hold value of matrix inverse
  set<-function(inv)  {                     ##define the set function to assign new value of matrix in parent environment
    x<<-inv
    inv<<-NULL                              ##reset inv to NULL in case of new matrix                                
  }
 
   get <- function() x                      ##define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse  ##assigns value of inv in parent environment
  getinverse <- function() inv              ##gets the value of inv where called
  list(set = set, get = get,
       setinverse = setinverse,             ##you need this in order to refer to the functions with the $ operator
       getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {          #### Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
