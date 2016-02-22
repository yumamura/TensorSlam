#' Refold

#'@export
#'@param mat WIP
#'@param dimToRecover WIP
#'@param mode WIP
#'@return WIP

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


