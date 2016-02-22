#' reconstruct MUNTD result

#'@export
#'@param res WIP
#'@return WIP
ReconstMUNTD <- function(res){

	idx <- length(res$As)
	core <- res$g
	for(i in 1:idx){
		core <- kModeProduct(core,res$As[[i]],i)
	}
	return(core)

}


