## makeCacheMatrix creates an object in the form of list where first it
## stores the initial matrix value (NULL at the beginning) and then the
## cached values calculated afterwards

makeCacheMatrix <- function(x = matrix()) {
	  ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve() 

  inv<-NULL
  set<-function(y){
     # use `<<-` to assign a value to an object in an environment 
     # different from the current environment. 

  	x<<-y
  	inv<<-NULL
	}

get<-function() x  #returns the value of the original matrix

##assings the cached matrix and stores it as inv so cacheSolve() can call it
setmatrix<-function(solve) inv<<- solve 

getmatrix<-function() inv ##returns the cached matrix to cacheSolve()

##list of the internal functions so a calling function can acess them
list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## function to calculate the matrix inverse that uses a cached value in case
## the matrix inverse has been previously calculated. The method allows to 
## decrease computational time since it does not have to calculate de inverse
## everytime it runs

cacheSolve <- function(x=matrix(), ...) { ##input x created by makeCacheMatrix()
	## @x: output of makeCacheMatrix()
      ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inv<-x$getmatrix() ##access the matrix x and gets the inverse value

    if(!is.null(inv)){   #if inverse has been calculated the returns message and the cached value
	# get it from the cache and skips the computation. 
      message("getting cached data")  
      return(inv) ##returns ends function cacheSolve()
    }

# otherwise, calculates the inverse (when x$getmatrix() is NULL)
    matrix<-x$get()
    inv<-solve(matrix, ...) ##stores the calculated matrix in inv

# sets the value of the inverse in the cache (makeCacheMatrix) via the setmatrix function
    x$setmatrix(inv)
    inv ##returns the inverse matrix
}