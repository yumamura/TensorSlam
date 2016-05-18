context('Tensor Refold')


test_that('3d Dense Tensor Refold',{
		  library(rTensor)
		  set.seed(123)

		  input <- matrix(runif(27),3,9)
		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3),1)
		  true <- k_fold(as.tensor(input),1,c(3,3,3))
		  expect_equal(as.array(ans),true@data) 


		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3),2)
		  true <- k_fold(as.tensor(input),2,c(3,3,3))
		  expect_equal(as.array(ans),true@data) 

		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3),3)
		  true <- k_fold(as.tensor(input),3,c(3,3,3))
		  expect_equal(as.array(ans),true@data) 


})

test_that('4d Dense Tensor Refold',{
		   library(rTensor)
		  set.seed(123)

		  input <- matrix(runif(81),3,27)
		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3,3),1)
		  true <- k_fold(as.tensor(input),1,c(3,3,3,3))
		  expect_equal(as.array(ans),true@data) 

		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3,3),2)
		  true <- k_fold(as.tensor(input),2,c(3,3,3,3))
		  expect_equal(as.array(ans),true@data) 

		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3,3),3)
		  true <- k_fold(as.tensor(input),3,c(3,3,3,3))
		  expect_equal(as.array(ans),true@data) 

		  ans <- Refold(as.simple_sparse_array(input),c(3,3,3,3),4)
		  true <- k_fold(as.tensor(input),4,c(3,3,3,3))
		  expect_equal(as.array(ans),true@data) 


})


test_that('3d Sparse Tensor Refold',{
		  true <- as.simple_sparse_array(array(0,c(3,3,3))) 
		  true[1,1,] <- 1
		  true[2,1,] <- 2
		  true[3,1,] <- 3

		  input <- as.simple_sparse_array(matrix(rep(c(1:3,rep(0,6)),3),3))
		  ans <- Refold(input,c(3,3,3),1)
		  expect_equal(as.array(ans),as.array(true))


		  input <- as.simple_sparse_array(matrix(c(rep(1:3,3),rep(0,18)),3,9,byrow=T))
		  ans <- Refold(input,c(3,3,3),2)
		  expect_equal(as.array(ans),as.array(true))


		  input <- as.simple_sparse_array(matrix(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3,9,byrow=F))
		  ans <- Refold(input,c(3,3,3),3)
		  expect_equal(as.array(ans),as.array(true))
})

test_that('4d Sparse Tensor Refold',{
		  true <- as.simple_sparse_array(array(0,c(3,3,3,3))) 
		  true[1,1,,] <- 1
		  true[2,1,,] <- 2
		  true[3,1,,] <- 3
		  input <- as.simple_sparse_array(matrix(rep(c(1:3,rep(0,6)),9),3))
		  ans <- Refold(input,c(3,3,3,3),1)
		  expect_equal(as.array(ans),as.array(true))

		  input <- as.simple_sparse_array(matrix(c(rep(1:3,9),rep(0,54)),3,27,byrow=T))
		  ans <- Refold(input,c(3,3,3,3),2)
		  expect_equal(as.array(ans),as.array(true))


		  input <- as.simple_sparse_array(matrix(rep(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3),3,27,byrow=F))
		  ans <- Refold(input,c(3,3,3,3),3)
		  expect_equal(as.array(ans),as.array(true))


		  input <- as.simple_sparse_array(matrix(rep(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3),3,27,byrow=F))
		  ans <- Refold(input,c(3,3,3,3),4)
		  expect_equal(as.array(ans),as.array(true))
})
