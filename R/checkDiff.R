#' checkDiff

#'@export
#'@param WIP
#'@return WIP


checkDiff <- function(Xnew,Xold){ #2つのテンソルを受け取り，その変化率を計算する 変化率は|Xnew-Xold|/|Xold| ノルムはフロベニウス
	if(0){
		Xnew <- array(1:8,dim=c(2,2,2))
		Xnew <- array(2,dim=c(2,2,2))
		Xold <- array(2,dim=c(2,2,2))
	}

	if(class(Xnew)=='simple_sparse_array')Xnew <- as.array(Xnew)
	if(class(Xold)=='simple_sparse_array')Xold <- as.array(Xold)

	if(class(Xnew)=='simple_triplet_matrix')Xnew <- as.matrix(Xnew)
	if(class(Xnew)=='simple_triplet_matrix')Xnew <- as.matrix(Xnew)

	XDiff <- abs(Xnew-Xold)
	child <- sqrt(sum(XDiff^2))

	mother <- sqrt(sum(Xold^2))

	rate <- child/mother
	return(rate)
}
