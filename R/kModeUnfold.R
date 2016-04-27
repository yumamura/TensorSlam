#' kModeUnfold

#'@export
#'@param tnsr simple_sparse_array
#'@param m mode index to unfold
#'@return 2-dimensional simple_sparse_array (not simple_triplet_matrix)

kModeUnfold <- function(tnsr,m){ #モードk 行列化
	if(class(tnsr)=='array'){
		mat <- t(apply(tnsr,m,cBind))
		return(mat)
	}
	if(class(tnsr)=='simple_sparse_array'){
		if(prod(dim(tnsr))==1){ #大きさの全積が1,実質スカラーの時
			if(as.array(tnsr)[1]!=0){
				mat <- matrix(tnsr$v)
			}else{
				mat <- matrix(0)
			}
			return(mat)
		}else{
			#インデックス変更時に使うテーブル
			hashMat <- dim(tnsr)
			hashMat <- rbind(dimSize=hashMat,dimIdx=1:length(dim(tnsr)))
			
			hashMatExc <- hashMat[,-m]
			hashMatExc <- rbind(hashMatExc,multiply=c(1,cumprod(hashMatExc['dimSize',])[1:(ncol(hashMatExc)-1)])) #各次元インデックスの倍率

			rowIdx <- tnsr$i[,m,drop=F] #変換後の行位置
			colIdxBase <- tnsr$i[,-m,drop=F] #列位置計算のためのIndex
			colIdxBase[,2:ncol(colIdxBase)] <- colIdxBase[,2:ncol(colIdxBase)]-1

			colIdx <- colIdxBase %*% hashMatExc['multiply',]

			mat <- tnsr
			mat$i <- cbind(rowIdx,colIdx,deparse.level=0)
			mat$v <- tnsr$v
			mat$dim <- as.vector(c(hashMat['dimSize',m],prod(hashMatExc['dimSize',])))
			return(mat)


		}

	}

}


