context('Tensor kModeUnfold')

test_that('3d Dense Tensor Unfold',{
		  library(rTensor)
		  set.seed(123)

		  arr <- array(runif(27),c(3,3,3))
		  ans <- kModeUnfold(as.simple_sparse_array(arr),1)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 1)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(arr),2)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 2)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(arr),3)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 3)
		  expect_equal(as.array(ans),true@data)


})


test_that('4d Dense Tensor Unfold',{
		  library(rTensor)
		  set.seed(123)

		  arr <- array(runif(81),c(3,3,3,3))
		  ans <- kModeUnfold(as.simple_sparse_array(arr),1)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 1)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(arr),2)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 2)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(arr),3)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 3)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(arr),4)
		  true <- k_unfold(tnsr = as.tensor(arr),m = 4)
		  expect_equal(as.array(ans),true@data)


		  })


test_that('3d Sparse Tensor Unfold',{
		  input <- array(0,c(3,3,3)) 
		  input[1,1,] <- 1
		  input[2,1,] <- 2
		  input[3,1,] <- 3
		  ans <- kModeUnfold(as.simple_sparse_array(input),1)
		  true <- k_unfold(tnsr = as.tensor(input),m = 1)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(input),2)
		  true <- k_unfold(tnsr = as.tensor(input),m = 2)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(input),3)
		  true <- k_unfold(tnsr = as.tensor(input),m = 3)
		  expect_equal(as.array(ans),true@data)



})

test_that('4d Sparse Tensor Unfold',{
		  input <- array(0,c(3,3,3,3)) 
		  input[1,1,,] <- 1
		  input[2,1,,] <- 2
		  input[3,1,,] <- 3

		  ans <- kModeUnfold(as.simple_sparse_array(input),1)
		  true <- k_unfold(tnsr = as.tensor(input),m = 1)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(input),2)
		  true <- k_unfold(tnsr = as.tensor(input),m = 2)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(input),3)
		  true <- k_unfold(tnsr = as.tensor(input),m = 3)
		  expect_equal(as.array(ans),true@data)

		  ans <- kModeUnfold(as.simple_sparse_array(input),4)
		  true <- k_unfold(tnsr = as.tensor(input),m = 4)
		  expect_equal(as.array(ans),true@data)

})
