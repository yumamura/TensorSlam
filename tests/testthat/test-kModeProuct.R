context('kModeProduct')

test_that('3d Tensor Product',{
		  library(rTensor)
		  tnsr <- array(1:(3*4*5),dim=c(3,4,5))
		  mat1 <- matrix(1:30,10,3)
		  mat2 <- matrix(1:40,10,4)
		  mat3 <- matrix(1:50,10,5)
		  true1 <- ttm(as.tensor(tnsr),mat1,1)@data
		  true2 <- ttm(as.tensor(tnsr),mat2,2)@data
		  true3 <- ttm(as.tensor(tnsr),mat3,3)@data

		  ans1 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat1),1))
		  ans2 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat2),2))
		  ans3 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat3),3))

		  expect_equal(ans1,true1)
		  expect_equal(ans2,true2)
		  expect_equal(ans3,true3)
})

test_that('4d Tensor Product',{
		  library(rTensor)
		  tnsr <- array(1:(2*3*4*5),dim=c(2,3,4,5))
		  mat1 <- matrix(1:20,10,2)
		  mat2 <- matrix(1:30,10,3)
		  mat3 <- matrix(1:40,10,4)
		  mat4 <- matrix(1:50,10,5)
		  true1 <- ttm(as.tensor(tnsr),mat1,1)@data
		  true2 <- ttm(as.tensor(tnsr),mat2,2)@data
		  true3 <- ttm(as.tensor(tnsr),mat3,3)@data
		  true4 <- ttm(as.tensor(tnsr),mat4,4)@data

		  ans1 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat1),1))
		  ans2 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat2),2))
		  ans3 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat3),3))
		  ans4 <- as.array(kModeProduct(as.simple_sparse_array(tnsr),as.simple_sparse_array(mat4),4))

		  expect_equal(ans1,true1)
		  expect_equal(ans2,true2)
		  expect_equal(ans3,true3)
		  expect_equal(ans4,true4)
})

