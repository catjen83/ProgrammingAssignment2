## makeCacheMatrix creates a list with the functions needed to to run cacheSolve:
## Two functions to fetch data and two to set their value 

## Make a special matrix object

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get<-function() x
        setinv<-function(mymatrix) i<<-mymatrix
        getinv<-function() i
        #return the functions we created  in a list 
        #so they can passed in arguments of another function (cacheSolve)
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function can be used with the makeCacheMatrix function to return the invese of matrix x
#

cacheSolve <- function(x, ...) {
        
        i<-x$getinv()
        #check to see if the inverse has already been calculated
        if(!is.null(i)){
                message("getting cached data")
                return(i) #if i is not null, return i
        }
        data<-x$get() #otherwise, use get to fetch the matrix, store in data variable
        
        ## Return a matrix that is the inverse of 'x'
        i<-solve(data,...) #get the inverse of the matrix we fetched in the last step 
        x$setinv(i) #set it
        i #return it
        }
