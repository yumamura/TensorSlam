
reconstTucker <- function(core,Wlist){

	#'@export
	#'@param WIP
	#'@return WIP
	for(i in 1:length(Wlist)){
		core <- kModeProduct(tnsr=core,mat=Wlist[[i]],m=i)
	}
	return(core)
}
