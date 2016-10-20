#' SVD with irlba

#'@export
#'@param mat WIP
#'@param rank WIP
#'@param nu WIP
#'@param nv WIP
#'@return WIP

smartsvd <- function(mat,rank=10,nu=3,nv=3){
	if(min(dim(mat))>rank+3){
		set.seed(0)
		res <- irlba(mat,nu = nu,nv = nv,maxit=10000)
	}else{
		res <- svd(mat)
	}
	return(res)
}
