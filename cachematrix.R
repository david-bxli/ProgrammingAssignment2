## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#initial m tobe NULL
	m<-NULL
	
	#set x inside of makeCacheMatrix and initialize m to be NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	
	#get value of x
	get<-function() x
	
	#set matrix to m.
	setMatrix<-function(matrix) m<<-matrix
	
	#get value of m
	getMatrix<-function() m
	
	#expose list of functions
	list(set=set, get=get, setMatrix = setMatrix, getMatrix =getMatrix)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	#try to get cached matrix
	m<-x$getMatrix()
	
	#if m is not null, which indicates m has been cached, return cached matrix
	if(!is.null(m)){
		print("getting cached data")
		return(m)
	}
	
	#in this case, m hasn't been cached, the matrix needs to be processed and cache in x
	data<-x$get()
	
	#inverse matrix
	m<-solve(data)
	
	#store inversed matrix to x which will be cached.
	x$setMatrix(m)
	
	#return m
	m
}
