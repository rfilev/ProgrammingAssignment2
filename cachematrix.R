## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

#function to create a square matrix - just to create a matrix with aleatory elements 
#parameter - number of lines
createSquareMatrix<-function(numberLines=numeric()){
  set.seed(1)
  repeat{
    m=matrix(c(sample(1:100,numberLines^2,replace=T)),nrow=numberLines,ncol=numberLines)
    if(det(m)!=0){
      break;
    }
  }  
  print("Original Matrix")
  print(m)
  return(m)
}


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set de variavel
  #similar a constructor of an object
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x #similar to a getter
  setInvMatrix <- function(invertedMatrix) m <<- invertedMatrix #it is a setter
  getInvMatrix <- function() m
  #list all functions inside the makeCacheMatrix function (or object)
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}
## calculate the Inverse Matrix or get it from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInvMatrix(m)
  m
}

#test result - unitary test
#just to check if everything is working - inform the number of lines to create the square matrix
#and all basic tests are done
checkExerciseCorrectness<-function(numberOfLines=numeric()){
  #running exercise basic commands
  aux<-createSquareMatrix(numberOfLines)
  object<-makeCacheMatrix(aux)
  result<-cacheSolve(object)
  print("Inverse Matrix is:")
  print(result)
  
  #####checking if it is correct
  ##create identity matrix
  identityMatrix<-diag(numberOfLines)]
  ##multiply inverse with original matrix
  temp<-result%*%aux
  #round result to check identity further
  temp<-round(temp,0)
  print("check indentity:")
  print(temp)
  ##if inverse multiplied by original equals identity, so inverse is rigth
  if(all.equal(temp,identityMatrix)){
    print("inverse matrix is rigth")
  }
  else{
    print("Ops... something is wrong")
  }
  #testing cache
  result<-cacheSolve(object)
  print("The cached matrix is:")
  print(result)
}