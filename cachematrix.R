## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## 역행렬 저장변수 
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## 저장된 행렬 가져옴
  setinv <- function(solved) inv <<- solved ## 역행렬 설정
  getinv <- function() inv ## 역행렬 반환
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## list 출력
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){ ## 이미 캐시에 있다면
    message("getting cached solve")
    return(inv)
  }
  data <- x$get() 
  inv <- solve(data, ...) ## 역행렬 구하는 함수
  x$setinv(inv) ## 설정
  inv ## 반환
}
