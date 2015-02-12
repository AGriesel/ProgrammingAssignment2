#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      #Set value of the vector
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      #Get value of the vector
      get<-function() x
      #Set the values of the matrix
      setmatrix<-function(solve) m<<- solve
      #Get the values of the matrix
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
      # Calculate the matrix created with the above function
      m<-x$getmatrix()
      # Check to see if it has already been calculated
      if(!is.null(m)){
            #If it exists, get the matrix from the cache and skip the computation
            message("getting cached data")
            return(m)
      }
      # Calculate the matrix
      matrix <- x$get() 
      m<-solve(matrix, ...)
      # Set the value of the matrix in the cache 
      x$setmatrix(m)
      m
}
