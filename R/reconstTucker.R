#' reconstruct Tucker Result

#'@export
#'@param WIP
#'@return WIP


reconstTucker <- function(core,Wlist){

	for(i in 1:length(Wlist)){
		core <- kModeProduct(tnsr=core,mat=Wlist[[i]],m=i)
	}
	return(core)
}
