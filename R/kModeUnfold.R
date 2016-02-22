kModeUnfold <- function(tnsr,m){ #モードk 行列化
	#'@export
	#'@param WIP
	#'@return WIP
	if(class(tnsr)=='array'){
		mat <- t(apply(tnsr,m,cBind))
		return(mat)
	}
	if(class(tnsr)=='simple_sparse_array'){
		if(prod(dim(tnsr))==1){
			if(as.array(tnsr)[1]!=0){
				mat <- matrix(tnsr$v)
			}else{
				mat <- matrix(0)
			}
			return(mat)



		}else{
			#                         print('tnsr')
			#                         print(as.array(tnsr))
			#                         print(tnsr$i)
			#                         print(tnsr$v)
			#                         print('tnsr dim')
			#                         print(dim(tnsr))
			#                         print('m')
			#                         print(m)
			if(ncol(tnsr$i)!=3){
				tnsr <- as.simple_sparse_array(as.array(tnsr))
			}
			mat.nrow <- dim(tnsr)[m]
			mat.ncol <- prod(dim(tnsr)[-m])
			mat.rowIdx <- tnsr$i[,m]
			mat.colIdx <- tnsr$i[,-m]
			if(class(mat.colIdx)=='integer'){
				mat.colIdx <- t(as.matrix(mat.colIdx))
			}
			if(class(mat.colIdx)=='numeric'){
				mat.colIdx <- t(as.matrix(mat.colIdx))
			}
			mat.colIdx[,2:ncol(mat.colIdx)] <- mat.colIdx[,2:ncol(mat.colIdx)]-1
			shift.index <- dim(tnsr)[-m]
			shift.index <- c(1,cumprod(shift.index))[1:(length(dim(tnsr))-1)]
			mat.colIdx <- mat.colIdx %*% shift.index
			
			tnsr$i <- cbind(mat.rowIdx,mat.colIdx)
			dimnames(tnsr$i) <- NULL
			tnsr$dim <- c(mat.nrow,mat.ncol)
			tnsr$dimnames <- NULL



			return(tnsr)
		}

	}

}


