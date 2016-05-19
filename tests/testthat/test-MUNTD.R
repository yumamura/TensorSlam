context('MUNTD')

test_that('HOSVD-MUNTD',{
		  tnsr <- as.simple_sparse_array(array(1:216,dim=c(6,6,6)))
		  res <- MUNTD(tnsr,core_dims=c(2,2,2),lra_ranks=c(4),method='FSTD')

		  expect_equal(dim(res$g),c(2,2,2)) 
		  expect_equal(dim(res$As[[1]]),c(6,2)) 
		  expect_equal(dim(res$As[[2]]),c(6,2)) 
		  expect_equal(dim(res$As[[3]]),c(6,2)) 

		  expect_equal(class(res$g),'array') 
		  expect_equal(class(res$As[[1]]),'matrix') 
		  expect_equal(class(res$As[[2]]),'matrix') 
		  expect_equal(class(res$As[[3]]),'matrix') 
})

test_that('FSTD-MUNTD',{
		  tnsr <- as.simple_sparse_array(array(1:1000,dim=c(10,10,10)))
		  res <- MUNTD(tnsr,core_dims=c(2,2,2),lra_ranks=c(3),method='FSTD')

		  expect_equal(dim(res$g),c(2,2,2)) 
		  expect_equal(dim(res$As[[1]]),c(10,2)) 
		  expect_equal(dim(res$As[[2]]),c(10,2)) 
		  expect_equal(dim(res$As[[3]]),c(10,2)) 

		  expect_equal(class(res$g),'array') 
		  expect_equal(class(res$As[[1]]),'matrix') 
		  expect_equal(class(res$As[[2]]),'matrix') 
		  expect_equal(class(res$As[[3]]),'matrix') 
})
