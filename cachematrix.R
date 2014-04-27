## a cacheMatrix contains a matrix
## the cacheSolve function inverts a cacheMatrix, caching the result.

## usage : 
# mymatrix <- replicate(10, rnorm(10))
# 
# mycachematrix <- makeCacheMatrix(mymatrix)
# 
# cacheSolve(mycachematrix)
# ### should print "no cached data"
#
# cacheSolve(mycachematrix)
# ### should print "getting cached data"
#
# all.equal(cacheSolve(mycachematrix), solve(mymatrix))
# ### should return true

# creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( stored_matrix = matrix()) {
  cached_inverted_matrix <<- NULL
  
  set <- function(new_matrix) {
    stored_matrix <<- new_matrix
    cached_inverted_matrix <<- NULL
  }
  get <- function() stored_matrix
  setcached_inverted_matrix <- function(inverted) cached_inverted_matrix <<- inverted
  getcached_inverted_matrix <- function() cached_inverted_matrix
  list(set = set, get = get,
       setcached_inverted_matrix = setcached_inverted_matrix,
       getcached_inverted_matrix = getcached_inverted_matrix)
}

##computes the inverse of a makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed),
#then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

  cached_inverted_matrix <- x$getcached_inverted_matrix()
  
  if(!is.null(cached_inverted_matrix)) {
    message("getting cached data")
    return(cached_inverted_matrix)
  }
  message("no cached data")
  
  data <- x$get()
  inverted <- solve(data, ...)
  x$setcached_inverted_matrix(inverted)
  inverted
}
