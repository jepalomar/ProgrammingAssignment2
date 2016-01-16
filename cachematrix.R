## In this code we have a special matrix object that can cache its inverse 
## (matrix is assumed to be invertible). We also have implemented a special solve
##  method (cacheSolve) to work with this matrix. This method cheks if the matrix has its inverse cached 
## in order to avoid calculating it again.
## 

## Matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
		inverseMatrix <- NULL
		set <- function(y) {
				x <<- y
				inverseMatrix <<- NULL
		}
		get <- function() x
		setInverse <- function(invMatrix) inverseMatrix <<- invMatrix
		getInverse <- function() inverseMatrix
		list(set = set, get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


## Solve method to calculate the inverse of a makeCacheMatrix object. If the inverse has been cached
## it just gets the cached value. If not, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        thisInverse <- x$getInverse()
        if(!is.null(thisInverse)) {
                message("getting cached inverse matrix")
                return(thisInverse)
        }
        data <- x$get()
        thisInverse <- solve(data, ...)
		message("Inverse calculated")
        x$setInverse(thisInverse)
        thisInverse
}

##########   EXAMPLE  OF WHAT YOU GET BY USING IT. NOTE THAT THE "Inverse calculated"  ######
##########   MESSAGE IS ONLY OBTAINED THE FIRST TIME WE TRY TO CALCULATE THE INVERSE   ######
#
#> mat<-matrix(c(1,3,5,6,7,13,2,3,4),nrow=3,ncol=3)
#> myMat<-makeCacheMatrix(mat)
#> a<-cacheSolve(myMat)
#Inverse calculated
#> a
#           [,1]       [,2]       [,3]
#[1,] -0.7333333  0.1333333  0.2666667
#[2,]  0.2000000 -0.4000000  0.2000000
#[3,]  0.2666667  1.1333333 -0.7333333
#> a<-cacheSolve(myMat)
#getting cached inverse matrix
#> a
#           [,1]       [,2]       [,3]
#[1,] -0.7333333  0.1333333  0.2666667
#[2,]  0.2000000 -0.4000000  0.2000000
#[3,]  0.2666667  1.1333333 -0.7333333



