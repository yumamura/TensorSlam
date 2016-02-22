ReconstMUNTD <- function(res){
	#'@export
	#'@param WIP
	#'@return WIP
	idx <- length(res$As)
	core <- res$g
	for(i in 1:idx){
		core <- kModeProduct(core,res$As[[i]],i)
	}
	return(core)

}


