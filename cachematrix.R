## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## According to the assignment, only the cacheSolve should calculate the inverse.
## Functions were tested using the following unit test
##
## Step 1: c=rbind(c(1, -1/4), c(-1/4, 1)) ## Create a matrix
## Step 2: a <- makeCacheMatrix() ## create a variable to hold the function
## Step 3: a$setmatrix(c) ## Matrix contained by variable c is cached
## Step 4: a$getmatrix() ## Returns original matrix to show it is cached
## Step 5: cacheSolve(a) ## Returns an inverse matrix without printed message to show inverse was not cached, but calculated
## Step 6: a$getinverse() ## Returns inverse matrix to show that cacheSolve cached the inverse matrix
## Step 7: cacheSolve(a) ## Returns inverse matrix with printed message to show that the cached inverse matrix was returned not a new calculation
##
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        setmatrix <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        #Returns list of objects, matrix (get, set) and inverse of matrix(get, set)
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse) 
}
## This function computes the inverse of the special "matrix"
cacheSolve <- function(x = matrix(), ...) 
{
        # Populate m with a cached inverve matrix
        m <- x$getinverse()
        # Check to see if we found a previously cached matrix
        if (!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        matrix <- x$getmatrix()
        m <- solve(matrix, ...)
        # As we have just calculated the inverse then cache the output
        x$setinverse(m)
        m ## Returns m
}