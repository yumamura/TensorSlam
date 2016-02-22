#' SVD with irlba

#'@export
#'@param WIP
#'@return WIP

smartsvd <- function(mat,rank=10,nu=3,nv=3){
	if(min(dim(mat))>rank+3){
		res <- irlba(mat,nu = nu,nv = nv)
	}else{
		res <- svd(mat)
	}
	return(res)
}
