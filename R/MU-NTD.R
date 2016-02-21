library(spam)
library(slam)
library(rTensor)
library(doMC)

if(file.exists('./TensorUtil.R')){
	source('./TensorUtil.R')
}else{
	source('~/Documents/Tensor/TensorEngine/TensorUtil.R')
}

if(file.exists('./FSTD.R')){
	source('./FSTD.R')
}else{
	source('~/Documents/Tensor/TensorEngine/FSTD.R')
}

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

MUNTD <- function(y,core_dims=ceiling(dim(y)/5),lra_ranks=core_dims,method='FSTD',convDebug=F){
	#'@export
	eps <- 2.2204e-16
	if(convDebug==T){ #更新回数判定用
		rA <- list(rA1=matrix(0,24,10),
		rA2 <- matrix(0,24,10),
		rA3 <- matrix(0,24,10))
		rG <- matrix(0,24,10)
		GA <- matrix(0,3,24)
		GG <- rep(0,24)

	}
	#         print('Perform LRA')
	if(method=='TensorUtil'){
	hosvd_res <- TensorUtil(y,lra_ranks,doMC=F)
	}else{
		hosvd_res <- FSTDFixedFNum(y,lra_ranks)
	}
	#         save(hosvd_res,file='/home/data/boru/hosvd_res.Rdata')

	#         y_approx <- reconstTensorUtil(hosvd_res) 
	#         save(y_approx,file='/home/data/boru/y_approx.Rdata')

	g_t <- hosvd_res$g
	As_t <- hosvd_res$As

	#         print('initialize core tensor')
	g <- array(runif(prod(core_dims)),core_dims)
	#         print('initialize A matrix')
	As <- lapply(1:length(dim(y)),function(mode.n){
			     n.col <- core_dims[mode.n]
			     n.row <- dim(y)[mode.n]
			     A <- as.simple_sparse_array(matrix(runif(n.col*n.row),n.row,n.col))
			     return(A)
})
	AtAs <- lapply(1:length(As),function(i){
			       AtA <- crossprod_simple_triplet_matrix(as.simple_triplet_matrix(As[[i]]),as.simple_triplet_matrix(As[[i]]))
			       return(AtA)
})
	AtyAs <- lapply(1:length(As),function(i){
				AtyA <- crossprod_simple_triplet_matrix(as.simple_triplet_matrix(As[[i]]),as.simple_triplet_matrix(As_t[[i]]))
				return(AtyA)
})


	if(1==0){
	library(R.matlab)
	tmp <- readMat('~/Downloads/ntdInit.mat')
	g_t <- tmp$aycore
	As_t <- list(tmp$ayu1,tmp$ayu2,tmp$ayu3)
	g <- tmp$acore
	As <- list(tmp$aa1,tmp$aa2,tmp$aa3)
	eps <- 2.2204e-16
	tol <- 1e-3
	AtAs <- lapply(1:length(As),function(i){
			       AtA <- crossprod_simple_triplet_matrix(as.simple_triplet_matrix(As[[i]]),as.simple_triplet_matrix(As[[i]]))
			       return(AtA)
})
	AtyAs <- lapply(1:length(As),function(i){
				AtyA <- crossprod_simple_triplet_matrix(as.simple_triplet_matrix(As[[i]]),as.simple_triplet_matrix(As_t[[i]]))
				return(AtyA)
})
	}
	#         print('Enter Main Loop')
	MAIN <- 1
	while(MAIN<25){ #TODO
		As_old <- As
		A10 <- As[[1]]

		for(d in 1:length(dim(y))){
			#                         print(paste0('Updating A of dimension ',d))
			mode <- 1:length(dim(g))
			mode <- mode[-d]
			x <- g
			for(i in mode){
				#                                 print(i)
				x <- kModeProduct(x,mat = AtAs[[i]],m = i)
			}
			Gnt <- kModeUnfold(g,d)
			x_t <- g_t
			for(i in mode){
				#                                 print(i)
				x_t <- kModeProduct(x_t,mat = AtyAs[[i]],m = i)
			}
			BtB <- kModeUnfold(x,d)
			BtB <- crossprod_simple_triplet_matrix(t(as.simple_triplet_matrix(BtB)),t(as.simple_triplet_matrix(Gnt)))
			YB <- kModeUnfold(x_t,d)
			YB <- crossprod_simple_triplet_matrix(t(as.simple_triplet_matrix(YB)),t(as.simple_triplet_matrix(Gnt)))
			YB <- crossprod_simple_triplet_matrix(t(as.simple_triplet_matrix(As_t[[d]])),as.simple_triplet_matrix(YB))
			A <- As[[d]]
			for(i in 1:10){
				A_old <- A
				A <- as.array(A) * YB
				mother <- as.array(A_old) %*% BtB
				mother[which(mother<eps)] <- eps
				A <- A/mother
				A[which(A<eps)] <- eps
				if(convDebug==T){
					rA[[d]][MAIN,i] <- checkDiff(A,A_old)
				}
				diffA <- checkDiff(A,A_old)
				if(d!=3&diffA<0.002)break;
				if(d==3&diffA<0.0003)break;
			}
			nrm <- apply(A,2,max)
			for(i in 1:ncol(A)){
				A[,i] <- A[,i]/nrm[i]
			}
			As[[d]] <- A
			g <- kModeProduct(as.simple_sparse_array(g),diag(nrm),d)
			AtAs[[d]] <- crossprod_simple_triplet_matrix(as.simple_triplet_matrix(As[[d]]),as.simple_triplet_matrix(As[[d]]))
			AtyAs[[d]] <- crossprod_simple_triplet_matrix(as.simple_triplet_matrix(As[[d]]),as.simple_triplet_matrix(As_t[[d]]))

		}
		if(convDebug==T){
			GA[1,MAIN] <- checkDiff(As[[1]],As_old[[1]])
			GA[2,MAIN] <- checkDiff(As[[2]],As_old[[2]])
			GA[3,MAIN] <- checkDiff(As[[3]],As_old[[3]])
		}
		#                 e <- try({if(max(abs(as.array(A10)-as.array(As[[1]])))<tol) break})
		#                 if(class(e)=='try-error')browser()

		d <- length(dim(y)) #d=Nとしているが，dを適当な1方向にしてよいのかはよくわからない
		enum <- kModeProduct(x_t,AtyAs[[d]],d)
		GG_old <- g
		for(i in 1:10){ #updating g
			g_old <- g
			mother <- as.simple_sparse_array(g)
			for(m in 1:length(AtAs)){
				mother <- kModeProduct(mother,AtAs[[m]],m)
			}
			mother$v[which(mother$v<eps)] <- eps
			right <- as.array(enum)/as.array(mother)
			g <- as.array(g)*right
			if(convDebug==T){
				rG[MAIN,i] <- checkDiff(g,g_old)
			}
			diffG <- checkDiff(g,g_old)
			if(diffG<0.001)break;

		}
		if(convDebug==T){
			GG[MAIN] <- checkDiff(g,GG_old)
		}
		diffGG <- checkDiff(g,GG_old)
		if(diffGG<0.005)break;

		MAIN <- MAIN+1

	}

	if(convDebug==F){
	return(list(g=g,As=As))
	}else{
		diffList <- list(rA,rG,GA,GG)
	return(list(g=g,As=As,diffList=diffList))
	}
}

ReconstMUNTD <- function(res){
	idx <- length(res$As)
	core <- res$g
	for(i in 1:idx){
		core <- kModeProduct(core,res$As[[i]],i)
	}
	return(core)

}

checkDiff <- function(Xnew,Xold){ #2つのテンソルを受け取り，その変化率を計算する 変化率は|Xnew-Xold|/|Xold| ノルムはフロベニウス
	if(0){
		Xnew <- array(1:8,dim=c(2,2,2))
		Xnew <- array(2,dim=c(2,2,2))
		Xold <- array(2,dim=c(2,2,2))
	}

	if(class(Xnew)=='simple_sparse_array')Xnew <- as.array(Xnew)
	if(class(Xold)=='simple_sparse_array')Xold <- as.array(Xold)

	if(class(Xnew)=='simple_triplet_matrix')Xnew <- as.matrix(Xnew)
	if(class(Xnew)=='simple_triplet_matrix')Xnew <- as.matrix(Xnew)

	XDiff <- abs(Xnew-Xold)
	child <- sqrt(sum(XDiff^2))

	mother <- sqrt(sum(Xold^2))

	rate <- child/mother
	return(rate)
}
