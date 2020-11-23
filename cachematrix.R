## These functions were created to create a special matrix, compute the inverse of that matrix, and cache that inverse. 

##This function can be used to create a matrix object that contains the four functions set_matrix(), get_matrix(),set_ivse(),and get_ivse() and the objects x and m. That is, the object created with this fucntion will contain the environemnt for makeCacheMatrix(). This function will assign the matrix to x, which is a "sibling" of the four functions set_matrix(), get_matrix(),set_ivse(),and get_ivse().

makeCacheMatrix <- function(x = matrix()) {
	ivse <- NULL

	set_matrix <- function(y){
##the <<- is used because x is an object in the parent environment and the input y is assigned to that x object in the parent environment.
	x <<- y
	ivse <<- NULL
}
##This function retrieves object x from the parent environment of makeCacheMatrix since object x is not defined in get_matrix()
    get_matrix <- function(){
		x
		}
		
	set_ivse <- function(inverse){
##Assigns the input inverse to the ivse object in the makeCacheMatrix() parent environment
		ivse <<- inverse 
	}
	
##This function retrives object ivse from the parent environment of make CacheMatrix since object ivse is not defined in get_ivse()
	get_ivse <- function(){
		ivse
		}
		
##Enambles the use of $ to access functions by name. Assigns each function as an element in a list and returns it to the parent environment. 
	list(set_matrix = set_matrix,get_matrix = get_matrix,set_ivse = set_ivse,get_ivse = get_ivse)
}



##This is a function that finds the inverse of an object makeCacheMatrix(). This function will use the get_ivse() to display the value of ivse. If the value is not NULL, then the inverse of the matrix is cached and the function will return that value. If the value if ivse is NULL, this functions calculates the inverse and uses set_ivse() to return ivse to the parent environment.

cacheSolve <- function(x, ...) {
##Retrives the inverse value and assigns it to the ivse object in the parent environment 
	ivse <- x$get_ivse()
	
##checks if the ivse object is not NULL, meaning it can skip the computation. If so, the inverse is then displayed by the line return(ivse) 
	if(!is.null(ivse)){
		message("Getting cached data")
		return(ivse)
	}else{
##The else contains code to solve for the inverse, because the else code would only be exceuted if the ivse object is NULL
		makeInverse <- x$get_matrix()
		ivse <- solve(makeInverse, ...)
		x$set_ivse(ivse)
		ivse
	}
}
