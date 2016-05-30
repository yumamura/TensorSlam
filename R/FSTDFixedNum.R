#' FSTD

#'@export
#'@param Y saimple_sparse_array (3rd-order tensor only)
#'@param k numeric. number of fibers to be picked up.
#'@return list. $g is core tensor array. $As is feature matrices' list.

FSTDFixedFNum <- function(Y,k){
	I <- dim(Y)
	N <- length(I)

	dat <- data.frame(Y$i,Y$v)
	indexTmp <- as.numeric(dat[which.max(dat[,N+1]),1:N]) #最初のFiberを選択
	index <- list()
	for(i in 1:length(indexTmp)){
		index[[i]] <- indexTmp[i]
	}
	rm(i)


	ssub <- rep(1,N)

	p <- 2

	while(p<=k){
		for(n in 1:N){
			if(p==2 && n==1){
				index[[n]] <- c(index[[n]],ceiling(runif(1,min=0,max=1)*I[n]))
				ssub[n] <- ssub[n]+1
			}else{
				index[[n]] <- c(index[[n]],inew(Yres,index[[n]]))
				ssub[n] <- ssub[n]+1
			}

			W <- Y[index[[1]],index[[2]],index[[3]]]
			Wpinv <- list()
			FIB <- list()
			for(m in 1:N){
				Wpinv[[m]] <- ginv(as.simple_triplet_matrix(kModeUnfold(tnsr = W,m=m)))
				ind <- index
				ind[[m]] <- 1:I[m]
				FIB[[m]] <- as.simple_triplet_matrix(kModeUnfold(eval(parse(text=extractArrayExpr('Y',ind))),m))
			}
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
			ind <- index
			ind[[nextInd]] <- 1:I[nextInd]
			FIBred[[nextInd]] <- FIB[[nextInd]]
			smat <- ssub
			smat[nextInd] <- I[nextInd]

			mother <- eval(parse(text=extractArrayExpr('Y',ind)))
			child <- reconstTucker(core = U,Wlist = FIBred)
			#diff
			mother <- as.array(mother)
			child <- as.array(child)
			tnsrDiff <- as.simple_sparse_array(mother-child)
			Yres <- kModeUnfold(tnsrDiff,nextInd)
		}
		p <- p+1
	}

	res <- list(g=U,As=FIB)

	return(res)
}


