#Peer assignment week 3
#Write two R functions, one to cache matrix, one to solve the matrix's inverse
#Reuben Anderson

## Caches matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL				#matrix cache variable, set to NULL
  set <- function(y) {	#set matrix function with input y variable 
    x <<- y			#set x to global y
    m <<- NULL		#set m to NULL
  }
  get <- function() x		#get matrix data, NOT the matrix ex. c(1,2,3,4,5,6,7,8,9) for a square matrix 
  setmatrix <- function(matrix) m <<- matrix #set m to the matrix
  getmatrix <- function() m	#get matrix from m where it is cached
  list(set = set, get = get,			#list to return for object makeCacheMatrix
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Solves matrix's inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(matrix(data,nrow= sqrt(length(data)),ncol= sqrt(length(data)), byrow=TRUE), tol = 1e-20)	#here is where it actually calculates inverse matrix
  x$setmatrix(m)
  m
}
