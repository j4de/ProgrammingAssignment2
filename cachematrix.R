## Two functions which aid efficiency when it comes to solving matrix 
## inversions.
## makeCacheMatrix to which you pass the matrix to be solved 
## (inverted) and cached.
## makeCacheMatrix returns the cached functions list which you then 
## can call cacheSolve passing the cached functions list for the 
## original matrix which will return the cached inverted matrix
##
## Once the inverted matrix is cached (makeCacheMatrix) you can 
## call cacheSolve repeatedly more efficiently than calling upon
## the R solve() function.
##
## see Usage example at the end of this R code file 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m		
}

## Usage guidelines based on RStudio help
## You can type the following to test the above functions
##
## ## Declare 'hilbert' function (from RStudio help)
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##
## ## Create square matrix (5 by 5)
## mat5 <- hilbert(5)
##
## ## Cache the matrix inversion (1st time)
## invmat5 <- makeCacheMatrix(mat5)
##
## ## Prove inverted matrix * orginal yields identity matrix
## round( cacheSolve(invmat5) %*% mat5 )
##
## round( cacheSolve(invmat5) %*% mat5 )
## getting cached data
## [,1] [,2] [,3] [,4] [,5]
## [1,]    1    0    0    0    0
## [2,]    0    1    0    0    0
## [3,]    0    0    1    0    0
## [4,]    0    0    0    1    0
## [5,]    0    0    0    0    1
##
## 

