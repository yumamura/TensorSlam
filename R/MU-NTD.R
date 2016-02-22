library(slam)
library(doMC)


if(1==0){
	y <- as.simple_sparse_array(array(1,c(20,40,60)))
	y <- array(1,c(20,40,60))
	yy <- as.tensor(y)
	#         tucker(yy,ranks=c(5,10,15))
	y[1,1:40,] <- 2
	core_dims <- c(5,10,15)
	lra_ranks <- core_dims

	library(R.matlab)
	tmp <- readMat('~/Downloads/ntdInit.mat')
	g_t <- tmp$aycore
	As_t <- list(tmp$ayu1,tmp$ayu2,tmp$ayu3)
	g <- tmp$acore
	As <- list(tmp$aa1,tmp$aa2,tmp$aa3)
	eps <- 2.2204e-16
	tol <- 1e-3


}

