kModeProduct <- function(tnsr,mat,m){


	#'@export
	#'@param WIP
	#'@return WIP
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
