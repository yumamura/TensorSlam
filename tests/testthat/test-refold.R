context('Tensor Refold')


test_that('3d Dense Tensor Refold',{
		 true <- as.simple_sparse_array(array(1:27,c(3,3,3))) 
		 input <- as.simple_sparse_array(matrix(1:27,3,9))
		 ans <- Refold(input,c(3,3,3),1)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

		 input <- as.simple_sparse_array(cbind(matrix(1:9,3,3,byrow=T),
						       matrix(10:18,3,3,byrow=T),
						       matrix(19:27,3,3,byrow=T)))
		 ans <- Refold(input,c(3,3,3),2)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

		 input <- as.simple_sparse_array(matrix(1:27,3,9,byrow=T))
		 ans <- Refold(input,c(3,3,3),3)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))
})

test_that('4d Dense Tensor Refold',{
		 true <- as.simple_sparse_array(array(1:81,c(3,3,3,3))) 
		 input <- as.simple_sparse_array(matrix(1:81,3))
		 ans <- Refold(input,c(3,3,3,3),1)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

		 input <- as.simple_sparse_array(cbind(matrix(1:9,3,3,byrow=T),
						       matrix(10:18,3,3,byrow=T),
						       matrix(19:27,3,3,byrow=T),
						       matrix(28:36,3,3,byrow=T),
						       matrix(37:45,3,3,byrow=T),
						       matrix(46:54,3,3,byrow=T),
						       matrix(55:63,3,3,byrow=T),
						       matrix(64:72,3,3,byrow=T),
						       matrix(73:81,3,3,byrow=T)))
		 ans <- Refold(input,c(3,3,3,3),2)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

		 input <- as.simple_sparse_array(cbind(matrix(1:27,3,9,byrow=T),
						       matrix(28:54,3,9,byrow=T),
						       matrix(55:81,3,9,byrow=T)))
		 ans <- Refold(input,c(3,3,3,3),3)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

		 input <- as.simple_sparse_array(matrix(1:81,3,,byrow=T))
		 ans <- Refold(input,c(3,3,3,3),4)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))
})


test_that('3d Sparse Tensor Refold',{
		 true <- as.simple_sparse_array(array(0,c(3,3,3))) 
		 true[1,1,] <- 1
		 true[2,1,] <- 2
		 true[3,1,] <- 3

		 input <- as.simple_sparse_array(matrix(rep(c(1:3,rep(0,6)),3),3))
		 ans <- Refold(input,c(3,3,3),1)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))


		 input <- as.simple_sparse_array(matrix(c(rep(1:3,3),rep(0,18)),3,9,byrow=T))
		 ans <- Refold(input,c(3,3,3),2)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))


		 input <- as.simple_sparse_array(matrix(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3,9,byrow=F))
		 ans <- Refold(input,c(3,3,3),3)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))
})

test_that('4d Sparse Tensor Refold',{
		  true <- as.simple_sparse_array(array(0,c(3,3,3,3))) 
		  true[1,1,,] <- 1
		  true[2,1,,] <- 2
		  true[3,1,,] <- 3
		 input <- as.simple_sparse_array(matrix(rep(c(1:3,rep(0,6)),9),3))
		 ans <- Refold(input,c(3,3,3,3),1)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

		 input <- as.simple_sparse_array(matrix(c(rep(1:3,9),rep(0,54)),3,27,byrow=T))
		 ans <- Refold(input,c(3,3,3,3),2)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))

	
		 input <- as.simple_sparse_array(matrix(rep(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3),3,27,byrow=F))
		 ans <- Refold(input,c(3,3,3,3),3)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))


		 input <- as.simple_sparse_array(matrix(rep(c(rep(1,3),rep(2,3),rep(3,3),rep(0,18)),3),3,27,byrow=F))
		 ans <- Refold(input,c(3,3,3,3),4)
		 expect_equal(dim(ans),dim(true)) 
		 expect_equal(dim(ans$i),dim(true$i)) 
		 expect_equal(table(ans$i),table(true$i)) 
		 expect_equal(sort(ans$v),sort(true$v))
		 expect_equal(as.array(ans),as.array(true))
})
