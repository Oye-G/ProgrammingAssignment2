## The aim of this assignment is to write a pair of functions that cache the inverse of a matrix. 

## The first function, makeCacheMatrix creates a special matrix object that
## is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(matrix) { ## This sets the matrix
              m <<- matrix
              i <<- NULL
      }
      
      get <- function() { ## This returns the matrix
              m
      }
      
      setInverse <- function(inverse) { ## Sets the inverse
                      i <<- inverse
      }
      
      getInverse <- function() { ## Here, an inverse of the matrix is returned
                      i
      }
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
      ## This returns the list of all the above methods.
}


## This function computes the inverse of the special matrix returned by the
## makeCacheMatrix above. If the matrix from above remains unchanged and its inverse
## has already been calculated, then use cacheSolve function below to retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      
      if(!is.null(m)) { ## Return the matrix inverse if already calculated
          message("getting cached inversed matrix")
          return(m)
      }
      
      data <- x$get() ## Grabs matrix
      m <- solve(data, ...) %*% data ## use matrix multiplication to calculate data (i.e. matrix) inverse
      x$setInverse(m) ## Sets inverse
      m ## Returns matrix
}
