MakeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  #Set the value of the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #Get the value of the matrix
  get<-function() x
  Set.Matrix = function(solve) m<<- solve
  Get.Matrix = function() m
  list(set = set, get = get,
       Set.Matrix = Set.Matrix,
       Get.Matrix = Get.Matrix)
}

cacheSolve <- function(x = matrix(), ...) {
  m = x$Get.Matrix()
  #Set the value of the inverse
  if(!is.null(m)){
    message("Retrieving you Cached Data")
    return(m)
  }
  #Get the value for the inverse
  matrix = x$get()
  m = solve(matrix, ...)
  x$Set.Matrix(m)
  m
}