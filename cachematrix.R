## A cache is a way to store objects in memory to accelerate subsequent 
## access to the same object.We demonstarte how to make functions more efficient
## when expensive computations require efficient coding to reduce the 
##computational load.This excercise is to demonstarte storing a function return
##value in cache, to use it further repeatedly as required saving computing time.
##cache to use to avoid recomputing throughout the program.We use the lexical 
##scoping of R to get this done.

## makeCacheMatrix()  builds a set of functions and returns the functions
## within a "list" to the parent environment.The input matrix=x defined in the 
## function argument. makeCacheMatrix(),due to lexical scoping, contains 
##a complete copy of the environment for makeCacheMatrix(), including any objects
##that are defined within makeCacheMatrix() at design time.
##The accessible objects within makeCacheMatrix() are: set(y),y,get(),
##setsolve(solve), solve,get(solve),x, invm
## The function to be executed as follows:1. Create a non-singular square matrix
## example: s<-matrix(rnorm(16),nrow=4,ncol=4)
## 2. run makeCacheMatrix() 3. a<-makeCacheMatrix(s) and 4.cacheSolve(a). 
## 5. The result will be an inverted matrix of "s". 
## 6. If you run cacheSolve(a) again,a message ""getting cached data"

  makeCacheMatrix <- function(x = matrix()) {
  
  invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
  
  get <- function() x
  setsolve <- function (solve) invm <<- solve
  getsolve <- function() invm
  list(set = set, get = get,
  setsolve = setsolve,
         getsolve = getsolve)
  
  }
## Access the data and functions from the parent environment of"makeCacheMatrix"
## and computes inverse of the matrix"s" and stores in catche. If re exicute the 
## function it will access the s-1 from cache and displays message"
## "getting cached data"

  cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getsolve()
    if(!is.null(invm)) {
      message("getting cached data")
      return(invm)
    }
  data <- x$get()
  invm<- solve(data, ...)
  x$setsolve(invm)
  invm
  }
  
 
  
