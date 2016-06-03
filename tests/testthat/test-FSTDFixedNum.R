context('FSTDFixedNum')

test_that('3d FSTD',{
		  tnsr <- array(1:1000,dim = c(10,10,10))
		  tnsr <- as.simple_sparse_array(tnsr)
		  res <- FSTDFixedFNum(tnsr,3)

		  expect_equal(dim(res$g),c(9,9,9))
		  expect_equal(dim(res$As[[1]]),c(10,9))
		  expect_equal(dim(res$As[[2]]),c(10,9))
		  expect_equal(dim(res$As[[3]]),c(10,9))
	
		  expect_equal(class(res$g),'simple_sparse_array')
		  expect_equal(class(res$As[[1]]),'simple_triplet_matrix')
		  expect_equal(class(res$As[[2]]),'simple_triplet_matrix')
		  expect_equal(class(res$As[[3]]),'simple_triplet_matrix')
			
		#TODO: FIX seed?
		 
})

test_that('4d FSTD',{
		  tnsr <- array(1:10000,dim = c(10,10,10,10))
		  tnsr <- as.simple_sparse_array(tnsr)
		  res <- FSTDFixedFNum(tnsr,3)

		  expect_equal(dim(res$g),c(27,27,27,27))
		  expect_equal(dim(res$As[[1]]),c(10,27))
		  expect_equal(dim(res$As[[2]]),c(10,27))
		  expect_equal(dim(res$As[[3]]),c(10,27))
		  expect_equal(dim(res$As[[4]]),c(10,27))
	
		  expect_equal(class(res$g),'simple_sparse_array')
		  expect_equal(class(res$As[[1]]),'simple_triplet_matrix')
		  expect_equal(class(res$As[[2]]),'simple_triplet_matrix')
		  expect_equal(class(res$As[[3]]),'simple_triplet_matrix')
			
})
