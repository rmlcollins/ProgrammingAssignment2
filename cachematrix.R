## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # 1. Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL # Reset the cached inverse when a new matrix is set
        }
        
        # 2. Get the value of the matrix
        get <- function() x
        
        # 3. Set the value of the inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # 4. Get the value of the inverse
        getInverse <- function() inv
        
        # Return a list of the 4 functions
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        # Check if the inverse is already cached
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If not cached, get the matrix data
        data <- x$get()
        
        # Calculate the inverse using the solve() function
        inv <- solve(data, ...)
        
        # Cache the calculated inverse
        x$setInverse(inv)
        
        # Return the inverse
        inv
}




# Source your file
source("cachematrix.R")

# Create a simple invertible matrix
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

# Get the matrix
my_matrix$get()

# Calculate the inverse (this should compute it)
cacheSolve(my_matrix)

# Call it again (this should print "getting cached data" and return the cached inverse)
cacheSolve(my_matrix)

