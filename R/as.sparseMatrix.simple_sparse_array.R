as.sparseMatrix.simple_sparse_array <- function(mat){

	#'@export
	#'@param WIP
	#'@return WIP
	mat <- sparseMatrix(i = mat$i[,1],j = mat$i[,2],x = mat$v,dims = mat$dim)
	return(mat)
}


