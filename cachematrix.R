## Najja Shakir Al-Islam
## Assignment 2 / Problem 1
## 1/22/2015


## Below are two functions that are written to first, cache a given invertible matrix, and then the second of the
## two functions is written to compute the inverse of the cached matrix.
## The purpose of constructing such functions is to make matrix operations less expensive.

											###############################

## The function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

											###############################

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 
	set <- function (y){
		x <<- y ## --> Assigns y to x in an environment that is one level up. 
		m <<- NULL ## --> m is undefined in an environment that is one level up. 
	}
get <- function () x ## Lexically scopes environments for the variable 
						  ## x and assigns the obtained object into "get."

setmatrix <- function (solve) ## Solves the Matrix equation: "Xm = y" 
									  ## for "m" and assigns the solution to "setmatrix."

m <<- solve ## --> Assigns the solution of the matrix to m, replacing its NULL value.

getmatrix <- function () m ## Lexically scopes environments for the variable m and assigns the obtained object 
								  ## into "getmatrix."

list (set = set, get = get, 
	setmatrix = setmatrix,
	getmatrix = getmatrix) ## Stores the contents of "set," "get," "setmatrix," and "getmatrix"
								 ## as a recursive vector.
}

											##############################
											
## The function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

											##############################
											
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix() ## --> Assigns the contents of "getmatrix" into "m."
		if (!is.null(m)){
			message("getting cached matrix") ## --> Returns TRUE if "m" is not NULL and TRUE.
			return (m)
		}
		matrix <- x$get() ## --> Assigns the contents of "get" into "matrix." 
		m <- solve (matrix, ...) ## --> Assigns the solution of "matrix" and assigns the solution to "m."
		x$setmatrix(m) ## --> Lists the content of "m" from "x."
		m ## --> Returns "m"-- the inverse of "x." 
}
