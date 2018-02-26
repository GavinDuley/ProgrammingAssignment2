## Put comments here that give an overall description of what your
## functions do

# example usage
# > matrix_ordinary <- matrix(c(5, 1, 0,3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
# example numbers from https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html
# > matrix_special <- makeCacheMatrix(matrix_ordinary)
# > cacheSolve(matrix_special)

## load the library matlib to get the inv() function
library(matlib)

## this function makes a matrix whose invse can be cached

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	# set value of matrix & clear old inverse
	set<- function(setvalue) {
		x <<- setvalue
		m <<- NULL
	}
	# this should get the value of the matrix
	get <- function() x
	# this function sets the inverse, used by getinverse()
	# if no cached inverse found
	set_Inv <- function(inverse) m <<- inverse
	# get the inverse
	get_Inv <- function() m
	
	# list of above
	list(set=set,get=get, set_Inv=set_Inv, get_Inv=get_Inv)
}


## This function returns the invese of the matrix 
## created by the previous function, unless it has
## already been calculated in which case it simply
## retrieves the pre-calculated result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$get_Inv() #retrieves cached value
	if(!is.null(m)){
		message("retrieving cached data")
		return(m)
	}
	# if cache was empty, calculate the inverse
	finallythematrix <- x$get()
	m <- inv(finallythematrix)
	x$set_Inv(m)
	m
}
