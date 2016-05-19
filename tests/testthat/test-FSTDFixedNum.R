context('FSTDFixedNum')

test_that('3d FSTD',{
		  tnsr <- array(1:216,dim = c(6,6,6))
		  tnsr <- as.simple_sparse_array(tnsr)
		  res <- FSTDFixedFNum(tnsr,2)

		  expect_equal(dim(res$g),c(4,4,4))
		  expect_equal(dim(res$As[[1]]),c(6,4))
		  expect_equal(dim(res$As[[2]]),c(6,4))
		  expect_equal(dim(res$As[[3]]),c(6,4))
	
		  expect_equal(class(res$g),'simple_sparse_array')
		  expect_equal(class(res$As[[1]]),'simple_triplet_matrix')
		  expect_equal(class(res$As[[2]]),'simple_triplet_matrix')
		  expect_equal(class(res$As[[3]]),'simple_triplet_matrix')
			

		 
})
