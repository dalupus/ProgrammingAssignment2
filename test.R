testCacheSolve <- function(){
  x <- matrix(rnorm(160000),400,400)
  x_ <- makeCacheMatrix(x)
  for (i in 1:1000) {
    x__ <- cacheSolve(x_)
  }
}

testSolve <- function(){
  x <- matrix(rnorm(160000),400,400)
  for (i in 1:1000) {
    x_ <- solve(x)
  }
}

testEquality <- function(){
  x <- matrix(rnorm(160000),400,400)
  x_ <- makeCacheMatrix(x)
  cacheSolve(x_)
  print(identical(x_$getInv(),solve(x)))
}

system.time(testCacheSolve())
system.time(testSolve())
testEquality()