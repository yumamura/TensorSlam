context('Tensor kModeUnfold')

test_that('3d Dense Tensor Unfold',{
		 input <- as.simple_sparse_array(array(1:27,c(3,3,3))) 
		 ans1 <- kModeUnfold(input,1)
		 true1 <- as.simple_sparse_array(matrix(1:27,3,9))
		 true1$dimnames <- NULL

		 #indexを定める順序が異なるので
		 expect_equal(dim(ans1),dim(true1)) 
		 expect_equal(dim(ans1$i),dim(true1$i)) 
		 expect_equal(table(ans1$i),table(true1$i)) 
		 expect_equal(sort(ans1$v),sort(true1$v))
		 expect_equal(as.array(ans1),as.array(true1))

		 ans2 <- kModeUnfold(input,2)
		 true2 <- as.simple_sparse_array(cbind(matrix(1:9,3,3,byrow=T),
						       matrix(10:18,3,3,byrow=T),
						       matrix(19:27,3,3,byrow=T)))
		 true2$dimnames <- NULL

		 expect_equal(dim(ans2),dim(true2)) 
		 expect_equal(dim(ans2$i),dim(true2$i)) 
		 expect_equal(table(ans2$i),table(true2$i)) 
		 expect_equal(sort(ans2$v),sort(true2$v))
		 expect_equal(as.array(ans2),as.array(true2))

		 ans3 <- kModeUnfold(input,3)
		 true3 <- as.simple_sparse_array(matrix(1:27,3,9,byrow=T))
		 true3$dimnames <- NULL

		 expect_equal(dim(ans3),dim(true3)) 
		 expect_equal(dim(ans3$i),dim(true3$i)) 
		 expect_equal(table(ans3$i),table(true3$i)) 
		 expect_equal(sort(ans3$v),sort(true3$v))
		 expect_equal(as.array(ans3),as.array(true3))
})


test_that('4d Dense Tensor Unfold',{
		 input <- as.simple_sparse_array(array(1:81,c(3,3,3,3))) 
		 ans1 <- kModeUnfold(input,1)
		 true1 <- as.simple_sparse_array(matrix(1:81,3))
		 true1$dimnames <- NULL
		 expect_equal(dim(ans1),dim(true1)) 
		 expect_equal(dim(ans1$i),dim(true1$i)) 
		 expect_equal(table(ans1$i),table(true1$i)) 
		 expect_equal(sort(ans1$v),sort(true1$v))
		 expect_equal(as.array(ans1),as.array(true1))

		 ans2 <- kModeUnfold(input,2)
		 true2 <- as.simple_sparse_array(cbind(matrix(1:9,3,3,byrow=T),
						       matrix(10:18,3,3,byrow=T),
						       matrix(19:27,3,3,byrow=T),
						       matrix(28:36,3,3,byrow=T),
						       matrix(37:45,3,3,byrow=T),
						       matrix(46:54,3,3,byrow=T),
						       matrix(55:63,3,3,byrow=T),
						       matrix(64:72,3,3,byrow=T),
						       matrix(73:81,3,3,byrow=T)))
		 true2$dimnames <- NULL
		 expect_equal(dim(ans2),dim(true2)) 
		 expect_equal(dim(ans2$i),dim(true2$i)) 
		 expect_equal(table(ans2$i),table(true2$i)) 
		 expect_equal(sort(ans2$v),sort(true2$v))
		 expect_equal(as.array(ans2),as.array(true2))

		 ans3 <- kModeUnfold(input,3)
		 true3 <- as.simple_sparse_array(cbind(matrix(1:27,3,9,byrow=T),
						       matrix(28:54,3,9,byrow=T),
						       matrix(55:81,3,9,byrow=T)))
		 true3$dimnames <- NULL
		 expect_equal(dim(ans3),dim(true3)) 
		 expect_equal(dim(ans3$i),dim(true3$i)) 
		 expect_equal(table(ans3$i),table(true3$i)) 
		 expect_equal(sort(ans3$v),sort(true3$v))
		 expect_equal(as.array(ans3),as.array(true3))

		 ans4 <- kModeUnfold(input,4)
		 true4 <- as.simple_sparse_array(matrix(1:81,3,,byrow=T))
		 true4$dimnames <- NULL
		 expect_equal(dim(ans4),dim(true4)) 
		 expect_equal(dim(ans4$i),dim(true4$i)) 
		 expect_equal(table(ans4$i),table(true4$i)) 
		 expect_equal(sort(ans4$v),sort(true4$v))
		 expect_equal(as.array(ans4),as.array(true4))
	})


test_that('3d Sparse Tensor Unfold',{
		 input <- as.simple_sparse_array(array(0,c(3,3,3))) 
		 input[1,1,] <- 1
		 input[2,1,] <- 2
		 input[3,1,] <- 3
		 ans1 <- kModeUnfold(input,1)
		 true1 <- as.simple_sparse_array(matrix(rep(c(1:3,rep(0,6)),3),3))
		 true1$dimnames <- NULL

		 #indexを定める順序が異なるので
		 expect_equal(dim(ans1),dim(true1)) 
		 expect_equal(dim(ans1$i),dim(true1$i)) 
		 expect_equal(table(ans1$i),table(true1$i)) 
		 expect_equal(sort(ans1$v),sort(true1$v))
		 expect_equal(as.array(ans1),as.array(true1))

		 ans2 <- kModeUnfold(input,2)
		 true2 <- as.simple_sparse_array(matrix(c(rep(1:3,3),rep(0,18)),3,9,byrow=T))
		 true2$dimnames <- NULL

		 expect_equal(dim(ans2),dim(true2)) 
		 expect_equal(dim(ans2$i),dim(true2$i)) 
		 expect_equal(table(ans2$i),table(true2$i)) 
		 expect_equal(sort(ans2$v),sort(true2$v))
		 expect_equal(as.array(ans2),as.array(true2))

		 ans3 <- kModeUnfold(input,3)
		 true3 <- as.simple_sparse_array(matrix(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3,9,byrow=F))
		 true3$dimnames <- NULL

		 expect_equal(dim(ans3),dim(true3)) 
		 expect_equal(dim(ans3$i),dim(true3$i)) 
		 expect_equal(table(ans3$i),table(true3$i)) 
		 expect_equal(sort(ans3$v),sort(true3$v))
		 expect_equal(as.array(ans3),as.array(true3))
})

test_that('4d Sparse Tensor Unfold',{
		 input <- as.simple_sparse_array(array(0,c(3,3,3,3))) 
		 input[1,1,,] <- 1
		 input[2,1,,] <- 2
		 input[3,1,,] <- 3
		 ans1 <- kModeUnfold(input,1)
		 true1 <- as.simple_sparse_array(matrix(rep(c(1:3,rep(0,6)),9),3))
		 true1$dimnames <- NULL

		 #indexを定める順序が異なるので
		 expect_equal(dim(ans1),dim(true1)) 
		 expect_equal(dim(ans1$i),dim(true1$i)) 
		 expect_equal(table(ans1$i),table(true1$i)) 
		 expect_equal(sort(ans1$v),sort(true1$v))
		 expect_equal(as.array(ans1),as.array(true1))

		 ans2 <- kModeUnfold(input,2)
		 true2 <- as.simple_sparse_array(matrix(c(rep(1:3,9),rep(0,54)),3,27,byrow=T))
		 true2$dimnames <- NULL

		 expect_equal(dim(ans2),dim(true2)) 
		 expect_equal(dim(ans2$i),dim(true2$i)) 
		 expect_equal(table(ans2$i),table(true2$i)) 
		 expect_equal(sort(ans2$v),sort(true2$v))
		 expect_equal(as.array(ans2),as.array(true2))

		 ans3 <- kModeUnfold(input,3)
		 true3 <- as.simple_sparse_array(matrix(rep(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3),3,27,byrow=F))
		 true3$dimnames <- NULL

		 expect_equal(dim(ans3),dim(true3)) 
		 expect_equal(dim(ans3$i),dim(true3$i)) 
		 expect_equal(table(ans3$i),table(true3$i)) 
		 expect_equal(sort(ans3$v),sort(true3$v))
		 expect_equal(as.array(ans3),as.array(true3))


		 ans4 <- kModeUnfold(input,4)
		 true4 <- as.simple_sparse_array(matrix(rep(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3),3,27,byrow=F))
		 true4$dimnames <- NULL

		 expect_equal(dim(ans4),dim(true4)) 
		 expect_equal(dim(ans4$i),dim(true4$i)) 
		 expect_equal(table(ans4$i),table(true4$i)) 
		 expect_equal(sort(ans4$v),sort(true4$v))
		 expect_equal(as.array(ans4),as.array(true4))
})
