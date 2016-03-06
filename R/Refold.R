#' Refold

#'@export
#'@param mat kModeUnfolded 2-dimensional simple_sparse_array (not simple_triplet_matrix)
#'@param dimToRecover size of tensor to make
#'@param mode mode of unfold
#'@return folded tensor (simple_sparse_array)

Refold <- function(mat,dimToRecover,mode){ #行列化されたやつを再びテンソルに
	kIdx <- mat$i[,1] #tnsr$i の mode モード目に入る 	
	colIdx <- mat$i[,2] #matの列位置 処理の主役
	max_itr <- length(dimToRecover)-1 #dim(tnsr)-1だけindexを戻す処理を繰り返す
	factr <- rev(c(1,cumprod(dimToRecover[-mode]))[-length(dimToRecover)])

	i <- 1
	indexMat <- c()
	while(i<=max_itr){
			index <- (colIdx -1)%/% factr[i] +1 #new index of mode i
			indexMat <- cbind(indexMat,index,deparse.level=0)
			sp <- colIdx %% factr[i]
			sp[which(sp==0)] <- factr[i]
			colIdx <- sp
			i <- i+1
	}
	indexMat <- indexMat[,rev(1:ncol(indexMat))]
	if(mode<=ncol(indexMat)){
	indexMat <- indexMat[,append(1:ncol(indexMat),values = mode,after= mode)] #kIdxのための場所確保
	indexMat[,mode] <- kIdx
	}else{
		indexMat <- cbind(indexMat,kIdx,deparse.level=0)
	}

	tnsr <- mat
	tnsr$i <- indexMat
	tnsr$v <- mat$v
	tnsr$dim <- dimToRecover
	return(tnsr)
}


