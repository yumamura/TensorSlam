context('HOSVD')

test_that({'3d small size HOSVD',
	library(rTensor)
	tnsr <- array(1:(4*5*6),c(4,5,6))
	true <- hosvd(as.tensor(tnsr),c(2,2,2))
	trueCore <- (true$Z)@data
	trueMatList <- true$U

	ans <- HOSVD(as.simple_sparse_array(tnsr),c(2,2,2))
	ansCore <- as.array(ans$g)
	ansMatList <- ans$As

	expect_identical(trueCore,ansCore)
	expect_identical(trueMatList,ansMatList)
})

test_that({'4d small size HOSVD',
	library(rTensor)
	tnsr <- array(1:(4*4*4*4),c(4,4,4,4))
	true <- hosvd(as.tensor(tnsr),c(2,2,2,2))
	trueCore <- (true$Z)@data
	trueMatList <- true$U

	ans <- HOSVD(as.simple_sparse_array(tnsr),c(2,2,2,2))
	ansCore <- as.array(ans$g)
	ansMatList <- ans$As

	expect_identical(trueCore,ansCore)
	expect_identical(trueMatList,ansMatList)
})
