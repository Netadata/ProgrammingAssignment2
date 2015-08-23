
## The first stage is to define a function which will generate and set the 
## initial matrix inverse to NULL and subsequently cache an inverse when 
## calculated  

makeCacheMatrix <- function(x = matrix()) {
       m_inv <- NULL                # set inverse to NULL
       set <- function(y) {         
             x <<- y
             m_inv <<- NULL
       }
       get <- function() x
       setinverse <- function(inverse) m_inv <<- inverse
       getinverse <- function() m_inv
       list(set = set, get = get,          # Create matrix object with the
            setinverse = setinverse,       # original matrix and its inverse
            getinverse = getinverse)
}

## cacheSolve will return the inverse of a matrix. 
## It first checks to see if the inverse is already stored in the cache and
## if so returns the cached inverse. Otherwise the inverse is calculated and
## returned. 

cacheSolve <- function(x, ...) {
      m_inv <- x$getinverse()
      if(!is.null(m_inv)) {         # Check if inverse already calculated 
            message("getting cached data")      
            return(m_inv)           # Return cached inverse
      }
      data <- x$get()
      m_inv <- solve(data,...)      # Otherwise calculate inverse
      x$setinverse(m_inv)
      m_inv                         # Return inverse
}

## Test data 
# test_matrix <- makeCacheMatrix(matrix(1:4,2,2))
# test_matrix$get()
# test_matrix$getinverse()
# cacheSolve(test_matrix)

# test_matrix_1 <- makeCacheMatrix(matrix(c(3,5,7,9),2,2))
# test_matrix_1$get()
# cacheSolve(test_matrix_1)
# cacheSolve(test_matrix)
