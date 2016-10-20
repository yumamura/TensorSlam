#' Calc HOSVD

#'@export
#'@param tnsr Tensor. simple_sparse_array
#'@param ranks ranks 
#'@param doMC  EXPERIMENTAL. Use doMC for parallel calculation or not.
#'@return list. $g is a core tensor (simple_sparse_array) $U is a list that contains matrix (matrix)
HOSVD <- function(tnsr,ranks=ceiling(dim(tnsr)/10),doMC=F){
	mode.n <- NULL

	#         if(1==0){
	#                 hosvd_res <- HOSVD(y,core_dims)
	#                 res <- hosvd(as.tensor(as.array(y)),core_dims)
	# 
	#                 ttm(as.tensor(as.array(g)),t(As[[1]]),1)@data
	#         }

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


