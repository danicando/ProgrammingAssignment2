## makeCacheMatrix creates a special "matrix" object 
## that cache the inverse of the original matrix. This special "matrix"
## is really a list of functions as follows:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## cached inverse matrix
    inv_matrix <- NULL 
    
    ##set function allows to set the matrix values
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
   
    ##get funtion retrieves the original matrix
    get <- function() x
   
    ##setinverse function caches the inverted matrix
    setinverse <- function(inverse) inv_matrix <<- inverse
   
    ##getinverse funtion retrieves the inverted matrix
    getinverse <- function() inv_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
   
    ## Firstly inverse matrix is loaded
    inv_matrix <- x$getinverse()

    ## If inverse matrix has been calcuted yet, 
    ##it retrieves a message and the matrix
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    
    ## If inverse matrix has not ever been calculated, it is calculated now 
    data <- x$get()
    inv_matrix <- solve(data, ...)
    
    ## Inverse matrix is cached
    x$setinverse(inv_matrix)
    
    ## Result is printed
    inv_matrix
}
