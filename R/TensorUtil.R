#HOSVD for simple_sparse_array (slam)
library(irlba)
library(Matrix)
library(slam)
library(rTensor)
library(dplyr)

if(1==0){
	sample_tnsr <- array(runif(10*20*30),c(10,20,30))
	sample_tnsr <- as.simple_sparse_array(sample_tnsr)
}

kModeUnfold <- function(tnsr,m){ #モードk 行列化

	if(class(tnsr)=='array'){
		mat <- t(apply(tnsr,m,cBind))
		return(mat)
	}
	if(class(tnsr)=='simple_sparse_array'){
		if(prod(dim(tnsr))==1){
			if(as.array(tnsr)[1]!=0){
				mat <- matrix(tnsr$v)
			}else{
				mat <- matrix(0)
			}
			return(mat)



		}else{
			#                         print('tnsr')
			#                         print(as.array(tnsr))
			#                         print(tnsr$i)
			#                         print(tnsr$v)
			#                         print('tnsr dim')
			#                         print(dim(tnsr))
			#                         print('m')
			#                         print(m)
			if(ncol(tnsr$i)!=3){
				tnsr <- as.simple_sparse_array(as.array(tnsr))
			}
			mat.nrow <- dim(tnsr)[m]
			mat.ncol <- prod(dim(tnsr)[-m])
			mat.rowIdx <- tnsr$i[,m]
			mat.colIdx <- tnsr$i[,-m]
			if(class(mat.colIdx)=='integer'){
				mat.colIdx <- t(as.matrix(mat.colIdx))
			}
			if(class(mat.colIdx)=='numeric'){
				mat.colIdx <- t(as.matrix(mat.colIdx))
			}
			mat.colIdx[,2:ncol(mat.colIdx)] <- mat.colIdx[,2:ncol(mat.colIdx)]-1
			shift.index <- dim(tnsr)[-m]
			shift.index <- c(1,cumprod(shift.index))[1:(length(dim(tnsr))-1)]
			mat.colIdx <- mat.colIdx %*% shift.index
			tnsr$i <- cbind(mat.rowIdx,mat.colIdx)
			tnsr$dim <- c(mat.nrow,mat.ncol)
			tnsr$dimnames <- NULL



			return(tnsr)
		}

	}

}

Refold <- function(mat,dimToRecover,mode){ #行列化されたやつを再びテンソルに
	#dimToRecoverの長さはtnsrの次元
	recoverOrder <- c(dimToRecover[mode],dimToRecover[-mode])
	mode.num <- c(mode,(1:length(dimToRecover))[-mode])
	index <- mat$i

	cumdim <- c(recoverOrder)[2:(length(dimToRecover))]
	cumdim <- cumprod(cumdim)
	for(i in 1:(length(cumdim))){
		if(i==1){
			tmp <- (index[,2])%%cumdim[i]
			tmp[which(tmp==0)] <- cumdim[i]
		}else{
			tmp <- (index[,2]-1) %/% cumdim[i-1] + 1
		}	
		index <- cbind(index,tmp)
	}
	index <- index[,c(-2)]
	if(class(index)=='numeric')index <- t(as.matrix(index))
	#         swap.idx <- 1:ncol(index)
	#         swap.idx <- swap.idx[-mode]
	#         swap.idx <- c(mode,swap.idx)
	tmp <- index[,1]
	index <- index[,-1]
	ncolIndex <- ncol(index)
	#         browser()
	if(is.null(ncolIndex)) ncolIndex <- length(index)
	if(mode==1){
		index <- cbind(tmp,index)
	}else if(mode==(ncolIndex+1)){
		index <- cbind(index,tmp)
	}else{
		if(class(index)=='numeric')index <- t(as.matrix(index))
		index <- cbind(index[,1:(mode-1)],tmp,index[,mode:ncolIndex])
	}

	#         

	#         browser()
	#         index <- index[,swap.idx]



	tnsr <- mat
	tnsr$i <- index
	tnsr$v <- mat$v
	tnsr$dim <- dimToRecover
	return(tnsr)
}

kModeProduct <- function(tnsr,mat,m){
	tnsr.mat <- kModeUnfold(tnsr,m)
	tnsr.mat <- as.simple_triplet_matrix(tnsr.mat)
	if(class(mat)!='simple_triplet_matrix') mat <- as.simple_triplet_matrix(mat)
	res.mat <- as.simple_sparse_array(crossprod_simple_triplet_matrix(t(mat),tnsr.mat))
	res.tnsr.dim <- dim(tnsr)
	res.tnsr.dim[m] <- nrow(mat)

	#         tnsr.mat <- as.simple_sparse_array(tnsr.mat)
	#         browser()
	res <- Refold(res.mat,res.tnsr.dim,m)
	return(res)
}








as.sparseMatrix.simple_sparse_array <- function(mat){
	mat <- sparseMatrix(i = mat$i[,1],j = mat$i[,2],x = mat$v,dims = mat$dim)
	return(mat)
}

smartsvd <- function(mat,rank=10,nu=3,nv=3){
	if(min(dim(mat))>rank+3){
		res <- irlba(mat,nu = nu,nv = nv)
	}else{
		res <- svd(mat)
	}
	return(res)
}

HOSVD <- function(tnsr,ranks=ceiling(dim(tnsr)/10),doMC=T){
	if(1==0){
		hosvd_res <- HOSVD(y,core_dims)
		res <- hosvd(as.tensor(as.array(y)),core_dims)

		ttm(as.tensor(as.array(g)),t(As[[1]]),1)@data
	}

	if(doMC==F){
		As <- lapply(1:length(dim(tnsr)),function(mode.n){
				     X.unfolded <- kModeUnfold(tnsr,mode.n)
				     X.unfolded <- as.sparseMatrix.simple_sparse_array(X.unfolded)
				     A <- smartsvd(X.unfolded,nu=ranks[mode.n])$u
				     A <- as.matrix(A[,1:ranks[mode.n]])
				     return(A)
})
	}else if(doMC==T){
		registerDoMC(3)
		As <- foreach(mode.n = 1:length(dim(tnsr)))%dopar%{
			X.unfolded <- kModeUnfold(tnsr,mode.n)
			X.unfolded <- as.sparseMatrix.simple_sparse_array(X.unfolded)
			A <- smartsvd(X.unfolded,nu=ranks[mode.n])$u
			A <- as.matrix(A[,1:ranks[mode.n]])
			return(A)
		}
	}
	g <- tnsr
	for(n in 1:length(dim(tnsr))){
		g <- kModeProduct(tnsr = g,mat = t(As[[n]]),m = n)
	}

	return(list(g=g,As=As))


}

reconstHOSVD <- function(hosvd_res){
	g <- hosvd_res$g
	for(n in 1:length(dim(g))){
		g <- kModeProduct(tnsr = g,mat=hosvd_res$As[[n]],m=n)
	}
	return(g)
}



reconstTucker <- function(core,Wlist){
	for(i in 1:length(Wlist)){
		core <- kModeProduct(tnsr=core,mat=Wlist[[i]],m=i)
	}
	return(core)
}
