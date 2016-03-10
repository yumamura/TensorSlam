#' checkDiff Dense Tensor only. DO NOT USE FOR HUGE TENSOR.

#'@export
#'@param Xnew WIP
#'@param Xold WIP
#'@return WIP


checkDiff <- function(Xnew,Xold){ #2つのテンソルを受け取り，その変化率を計算する 変化率は|Xnew-Xold|/|Xold| ノルムはフロベニウス


	if(class(Xnew)=='simple_sparse_array')Xnew <- as.array(Xnew)
	if(class(Xold)=='simple_sparse_array')Xold <- as.array(Xold)

	if(class(Xnew)=='simple_triplet_matrix')Xnew <- as.matrix(Xnew)
	if(class(Xold)=='simple_triplet_matrix')Xold <- as.matrix(Xold)

	XDiff <- abs(Xnew-Xold)
	child <- sqrt(sum(XDiff^2))

	mother <- sqrt(sum(Xold^2))

	rate <- child/mother #mother=0でINFになるな
	return(rate)
}
