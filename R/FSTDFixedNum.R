#' FSTD

#'@export
#'@param Y WIP
#'@param k WIP
#'@return WIP



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



