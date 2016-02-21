if(file.exists('./HOSVD.R')){
	source('./HOSVD.R')
}else{
	source('~/Documents/Tensor/TensorEngine/HOSVD.R')
}
library(slam)
library(MASS)

if(1==0){
	load('~/Dropbox/Labtmp/tnsr/0d_20141201_sanihour44.Rdata')
	load('/home/data/incident/murakami/NTDvsNMF/TnsrDataMorto0708id20.Rdata')
	ranks <- c(3,3,3)
	library(R.matlab)
	Y <- readMat(con = '~/Downloads/Y.mat')
	Y <- readMat(con = '~/Documents/MATLAB/Y.mat')
	Y <- Y$Y
	library(slam)
	Y <- as.simple_sparse_array(Y)
	k <- 5
	eps <- 0.0001

}
inew <- function(C,old_ind){
	n <- dim(C)[1]
	vec <- rep(0,n)
	for(i in 1:n){
		vec[i] <- sqrt(sum((C[i,]$v)^2))
	}
	ind <- order(abs(vec),decreasing=T)
	if(!any(old_ind==ind[1])){
		ind <- ind[1]
	}else if(!any(old_ind==ind[2])){
		ind <- ind[2]
	}else if(!any(old_ind==ind[3])){
		ind <- ind[3]
	}else if(!any(old_ind==ind[4])){
		ind <- ind[4]
	}else if(!any(old_ind==ind[5])){
		ind <- ind[5]
	}else if(!any(old_ind==ind[6])){
		ind <- ind[6]
	}else if(!any(old_ind==ind[7])){
		ind <- ind[7]
	}else if(!any(old_ind==ind[8])){
		ind <- ind[8]
	}else if(!any(old_ind==ind[9])){
		ind <- ind[9]
	}else if(!any(old_ind==ind[10])){
		ind <- ind[10]
	}else{
		ind <- ind[11]
	}
	return(ind)
}

FSTD <- function(Y,k,eps=NULL){
	I <- dim(Y)
	N <- length(I)

	index <- lapply(1:N,function(n){
				ceiling(runif(1,min=0,max=1)*I[n])
})
	ssub <- rep(1,N)

	p <- 2
	completed <- rep(0,N)

	while(p<=k && (prod(completed)==0)){
		#                 browser()
		#                 print(paste0('p=',p))
		for(n in 1:N){
			if(completed[n]==0){
				if(p==2 && n==1){
					index[[n]] <- c(index[[n]],ceiling(runif(1,min=0,max=1)*I[n]))
					ssub[n] <- ssub[n]+1
				}else{
					YresNorm <- sqrt(sum((Yres$v)^2))
					FIBredNorm <- sqrt(sum((FIBred[[nextInd]]$v)^2))
					if((YresNorm/FIBredNorm)>eps){ #WIP
						index[[n]] <- c(index[[n]],inew(Yres,index[[n]]))
						ssub[n] <- ssub[n]+1
						#                                                 print(paste0('ranks=',ssub))

					}else{
						#                                                 completed[n] <- 1
					}
				}

				W <- Y[index[[1]],index[[2]],index[[3]]]
				Wpinv <- list()
				FIB <- list()
				for(m in 1:N){
					Wpinv[[m]] <- ginv(as.simple_triplet_matrix(kModeUnfold(tnsr = W,m=m)))
					ind <- index
					ind[[m]] <- 1:I[m]
					FIB[[m]] <- as.simple_triplet_matrix(kModeUnfold(Y[ind[[1]],ind[[2]],ind[[3]]],m))
				}
				#                                 print('FIB')
				#                                 print(FIB)
				U <- reconstTucker(core = W,Wlist = Wpinv)
			}
			if(n==N){
				nextInd <- 1
			}else{
				nextInd <- n+1
			}

			if(completed[nextInd]==0){
				FIBred <- list() 
				for(m in 1:N){
					FIBred[[m]] <- FIB[[m]][index[[m]],]
				}
				#                                 print('FIBred')
				#                                 print(FIBred)
				ind <- index
				ind[[nextInd]] <- 1:I[nextInd]
				FIBred[[nextInd]] <- FIB[[nextInd]]
				smat <- ssub
				smat[nextInd] <- I[nextInd]

				mother <- Y[ind[[1]],ind[[2]],ind[[3]]]
				child <- reconstTucker(core = U,Wlist = FIBred)
				#diff
				mother <- as.array(mother)
				child <- as.array(child)
				tnsrDiff <- as.simple_sparse_array(mother-child)
				Yres <- kModeUnfold(tnsrDiff,nextInd)
			}
		}
		p <- p+1
	}
	#         browser()

	res <- list(g=U,As=FIB)

	return(res)
}



FSTDFixedFNum <- function(Y,k){
	I <- dim(Y)
	N <- length(I)
	# 
	#         index <- lapply(1:N,function(n){
	#                                 ceiling(runif(1,min=0,max=1)*I[n])
	# })
	dat <- data.frame(Y$i,Y$v)
	indexTmp <- as.numeric(dat[which.max(dat[,4]),1:3])
	index <- list()
	for(i in 1:length(indexTmp)){
		index[[i]] <- indexTmp[i]
	}
	rm(i)

	#         index <- list()
	#         index[[1]] <- 21
	#         index[[2]] <- 11188
	#         index[[3]] <- 1901



	ssub <- rep(1,N)

	p <- 2
	#         completed <- rep(0,N)

	#         while(p<=k && (prod(completed)==0)){
	while(p<=k){
		#                 browser()
		#                 print(paste0('p=',p))
		for(n in 1:N){
			if(p==2 && n==1){
				index[[n]] <- c(index[[n]],ceiling(runif(1,min=0,max=1)*I[n]))
				ssub[n] <- ssub[n]+1
			}else{
				index[[n]] <- c(index[[n]],inew(Yres,index[[n]]))
				ssub[n] <- ssub[n]+1
				#                                 print(paste0('ranks=',ssub))
			}

			W <- Y[index[[1]],index[[2]],index[[3]]]
			#                         print(index)
			Wpinv <- list()
			FIB <- list()
			for(m in 1:N){
				Wpinv[[m]] <- ginv(as.simple_triplet_matrix(kModeUnfold(tnsr = W,m=m)))
				ind <- index
				ind[[m]] <- 1:I[m]
				FIB[[m]] <- as.simple_triplet_matrix(kModeUnfold(Y[ind[[1]],ind[[2]],ind[[3]]],m))
			}
			#                                 print('FIB')
			#                                 print(FIB)

			#                         browser()
			U <- reconstTucker(core = W,Wlist = Wpinv)
			if(n==N){
				nextInd <- 1
			}else{
				nextInd <- n+1
			}

			FIBred <- list() 
			for(m in 1:N){
				FIBred[[m]] <- FIB[[m]][index[[m]],]
			}
			#                                 print('FIBred')
			#                         print(lapply(FIB,function(i)as.array(i)))
			ind <- index
			ind[[nextInd]] <- 1:I[nextInd]
			FIBred[[nextInd]] <- FIB[[nextInd]]
			smat <- ssub
			smat[nextInd] <- I[nextInd]

			mother <- Y[ind[[1]],ind[[2]],ind[[3]]]
			child <- reconstTucker(core = U,Wlist = FIBred)
			#diff
			mother <- as.array(mother)
			child <- as.array(child)
			tnsrDiff <- as.simple_sparse_array(mother-child)
			Yres <- kModeUnfold(tnsrDiff,nextInd)
		}
		p <- p+1
	}
	#         browser()

	res <- list(g=U,As=FIB)

	return(res)
}







if(1==0){
	while(class(e)=='try-error'){
		e <- try({
			res <- FSTDFixedFNum(Y,4)
		})
	}
}



