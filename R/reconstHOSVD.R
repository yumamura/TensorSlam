reconstHOSVD <- function(hosvd_res){

	#'@export
	#'@param WIP
	#'@return WIP
	g <- hosvd_res$g
	for(n in 1:length(dim(g))){
		g <- kModeProduct(tnsr = g,mat=hosvd_res$As[[n]],m=n)
	}
	return(g)
}


