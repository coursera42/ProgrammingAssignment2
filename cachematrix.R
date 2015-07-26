## this cached solve(inversing) for matrices.
## this is programming assignment no2 of https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions


## create a inverematrix function with getter&setter
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## returns a inversed matrix, caching the results to speed up multiple calls on the same matrix.
cacheSolve <- function(x, ...) {

    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    i
}
