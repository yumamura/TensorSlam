context('Tensor kModeUnfold')

test_that('3d Tensor Unfold',{
		 input <- as.simple_sparse_array(array(1:27,c(3,3,3))) 
		 ans1 <- kModeUnfold(input,1)
		 true1 <- as.simple_sparse_array(matrix(1:27,3,9))
		 true1$dimnames <- NULL

		 expect_equal(ans1,true1)
		 #TODO: k=2,3の時のテスト
		 #TODO: 4次元以上のテンソルに対するテスト
})

