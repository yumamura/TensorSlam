context('checlDiff')

test_that('array vs array',{
		  Xold <- array(1,c(4,4,4))
		  Xnew <- array(0,c(4,4,4))
		  expect_identical(checkDiff(Xnew,Xold),1)

		  Xold <- array(runif(4*4*4),c(4,4,4))
		  Xnew <- Xold
		  expect_identical(checkDiff(Xnew,Xold),0)

		  Xold <- array(2,c(4,4,4))
		  Xnew <- array(1,c(4,4,4))
		  expect_identical(checkDiff(Xnew,Xold),0.5)

		  Xold <- array(-1,c(4,4,4))
		  Xnew <- array(1,c(4,4,4))
		  expect_identical(checkDiff(Xnew,Xold),2)

		  Xold <- array(1,c(4,4,4))
		  Xnew <- array(-1,c(4,4,4))
		  expect_identical(checkDiff(Xnew,Xold),2)
})
