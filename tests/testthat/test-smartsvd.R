context('smartsvd')

test_that('small size SVD',{
		  mat <- matrix(1:25,5,5)
		  true <- svd(mat)
		  ans <- smartsvd(mat)
		  expect_identical(true,ans)
})

test_that('bigger size SVD with irlba',{
		  mat <- matrix(1:49,7,7)
		  true <- irlba(mat,nu=3,nv=3)
		  ans <- smartsvd(mat,rank=1,nu=3,nv=3)
		  expect_equal(true$d,ans$d) #check only eigen
})
